package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.parser.projections._
import scalanlp.optimize._

import scalanlp.trees._
import scalanlp.config.Configuration
import scalanlp.util.{ConsoleLogging, Log}
import splitting.StateSplitting
import scalala.tensor.counters.LogCounters
import scalanlp.parser.UnaryRuleClosure.UnaryClosureException
import InsideOutside._
import java.io._;
import ParseChart.LogProbabilityParseChart;
import scalanlp.concurrent.ParallelOps._;
import scalanlp.concurrent.ThreadLocal;
import scalala.Scalala._;
import scalala.tensor.counters.Counters._
import scalanlp.util._;

/**
 * 
 * @author dlwh
 */
class LatentDiscrimObjective[L,L2,W](featurizer: Featurizer[L2,W],
                            unsplitRoot: L, // self explanatory
                            trees: IndexedSeq[(BinarizedTree[L],Seq[W],SpanScorer)],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                            openTags: Set[L],
                            splitLabel: L=>Seq[L2],
                            unsplit: (L2=>L)) extends BatchDiffFunction[Int,DenseVector] {

  val root = {
    val splitRoot = splitLabel(unsplitRoot);
    require(splitRoot.length == 1, "Split of root must be length 1");
    splitRoot.head;
  }

  val indexedFeatures: FeatureIndexer[L2,W] = {
    val initGrammar = coarseParser.grammar;
    val initLex = coarseParser.lexicon;
    FeatureIndexer[L,L2,W](featurizer, initGrammar, initLex, splitLabel);
  }

  val indexedProjections = new ProjectionIndexer(coarseParser.grammar.index,
    indexedFeatures.labelIndex, unsplit);

  // This span scorer is for coarse grammar, need to project to fine scorer
  def projectCoarseScorer(coarseScorer: SpanScorer):SpanScorer ={
    new ProjectingSpanScorer(indexedProjections, coarseScorer);
  }

  val fullRange = (0 until trees.length);

  val treesWithCharts = (trees).par.map { case (tree,words,scorer) =>
    (tree,words,projectCoarseScorer(scorer));
  }

  def extractViterbiParser(weights: DenseVector) = {
    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val builder = CKYChartBuilder(root, lexicon, grammar);
    val parser = new ChartParser(builder,new ViterbiDecoder(indexedProjections));
    parser
  }

  def extractLogProbParser(weights: DenseVector)= {
    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb);
    parser
  }

  var numFailures = 0;

  def calculate(weights: DenseVector, sample: IndexedSeq[Int]) = {
    assert(weights.forall(!_._2.isNaN),"wtf weights: " + indexedFeatures.decode(weights));

    try {
      val parser = new ThreadLocal(extractLogProbParser(weights));
      val trees = sample.map(treesWithCharts);
      val startTime = System.currentTimeMillis();
      val ecounts = trees.par.fold(new ExpectedCounts[W](parser().grammar)) { (counts, treeWordsScorer) =>
        val localIn = System.currentTimeMillis();
        val (tree,words,spanScorer) = treeWordsScorer;
        try {

          val treeCounts = treeToExpectedCounts(parser().grammar,parser().lexicon,tree,words, spanScorer);
          val treeTime = System.currentTimeMillis() - localIn;
          val wordCounts = wordsToExpectedCounts(words, parser(), spanScorer);
          val wordTime = System.currentTimeMillis() - localIn;
          //println("Parse: " + words.length + " tree:" + treeTime + " parse: " + wordTime);
          counts += treeCounts -= wordCounts;
        } catch {
          case e => println("Error in parsing: " + words + e); e.printStackTrace(); throw new RuntimeException("Error parsing " + words,e);
        }
      } { (ecounts1, ecounts2) =>
        ecounts1 += ecounts2
      }
      val finishTime = System.currentTimeMillis() - startTime;

      println("Parsing took: " + finishTime / 1000.0)
      val grad = -expectedCountsToFeatureVector(ecounts) value;

      println((norm(grad,2), ecounts.logProb));
      assert(grad.forall(!_._2.isInfinite), "wtf grad");
      (-ecounts.logProb,  grad);
    }  catch {
      case ex: UnaryClosureException =>
        numFailures += 1;
        println(indexedFeatures.decode(weights));
        if(numFailures > 10) throw ex;
        ex.printStackTrace();
        (Double.PositiveInfinity,indexedFeatures.mkDenseVector(0.0));
    }

  }

  val splitOpenTags = {
    for(t <- openTags; s <- splitLabel(t))  yield s;
  }

  def weightsToLexicon(weights: DenseVector) = {
    val grammar = new FeaturizedLexicon(splitOpenTags, weights, indexedFeatures);
    grammar;
  }

  def weightsToGrammar(weights: DenseVector):Grammar[L2] = {
    val grammar =  new FeaturizedGrammar(weights,indexedFeatures)
    grammar;
  }

  def wordsToExpectedCounts(words: Seq[W],
                            parser: ChartBuilder[LogProbabilityParseChart,L2,W],
                            spanScorer: SpanScorer = SpanScorer.identity) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words, spanScorer);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  private def treeToExpectedCounts(g: Grammar[L2],
                                   lexicon: Lexicon[L2,W],
                                   t: BinarizedTree[L],
                                   words: Seq[W],
                                   spanScorer: SpanScorer = SpanScorer.identity):ExpectedCounts[W] = {
    StateSplitting.expectedCounts(g,lexicon,t.map(splitLabel),words,spanScorer);
  }

  def expectedCountsToFeatureVector(ecounts: ExpectedCounts[W]):DenseVector = {
    val result = indexedFeatures.mkDenseVector(0.0);

    // binaries
    for( (a,bvec) <- ecounts.binaryRuleCounts;
         (b,cvec) <- bvec;
         (c,v) <- cvec.activeElements) {
      result += (indexedFeatures.featuresFor(a,b,c) * v);
    }

    // unaries
    for( (a,bvec) <- ecounts.unaryRuleCounts;
         (b,v) <- bvec.activeElements) {
      result += (indexedFeatures.featuresFor(a,b) * v);
    }

    // lex
    for( (a,ctr) <- ecounts.wordCounts;
         (w,v) <- ctr) {
      result += (indexedFeatures.featuresFor(a,w) * v);
    }

    result;
  }

  def initialWeightVector = {
    val result = indexedFeatures.mkDenseVector(0.0);
    for(f <- 0 until result.size) {
      result(f) = indexedFeatures.initialValueFor(f);
    }
    result;
  }


}

object LatentDiscriminativeTrainer extends ParserTrainer {
  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i);
  }

  def unsplit(x: (String,Int)) = x._1;

  override def loadTrainSpans(config: Configuration):Iterable[SpanScorer] = {
    val spanDir = config.readIn[File]("spans.labeled",null);
    if(spanDir eq null) super.loadTrainSpans(config);
    else {
      val spanFile = new File(spanDir,ProjectTreebankToLabeledSpans.TRAIN_SPANS_NAME)
      ProjectTreebankToLabeledSpans.loadSpansFile(spanFile);
    }
  }
  override def loadDevSpans(config: Configuration):Iterable[SpanScorer] = {
    val spanDir = config.readIn[File]("spans.labeled",null);
    if(spanDir eq null) super.loadDevSpans(config);
    else {
      val spanFile = new File(spanDir,ProjectTreebankToLabeledSpans.DEV_SPANS_NAME)
      ProjectTreebankToLabeledSpans.loadSpansFile(spanFile);
    }
  }

  override def loadTestSpans(config: Configuration):Iterable[SpanScorer] = {
    val spanDir = config.readIn[File]("spans.labeled",null);
    if(spanDir eq null) super.loadTestSpans(config);
    else {
      val spanFile = new File(spanDir,ProjectTreebankToLabeledSpans.TEST_SPANS_NAME)
      ProjectTreebankToLabeledSpans.loadSpansFile(spanFile).map(obj.projectCoarseScorer _);
    }
  }

  var obj: LatentDiscrimObjective[String,(String,Int),String] = null;

  def quickEval(devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)], weights: DenseVector, iter: Int, iterPerValidate:Int) {
    ProjectTreebankToLabeledSpans.writeObject((weights,obj.indexedFeatures.decode(weights)),new java.io.File("weights.ser"));
    if(iter % iterPerValidate == 0) {
      println("Validating...");
      val parser = obj.extractViterbiParser(weights);
      val fixedTrees = devTrees.take(400).map { case (a,b,c) => (a,b,obj.projectCoarseScorer(c))}.toIndexedSeq;
      val results = ParseEval.evaluate(fixedTrees, parser, unaryReplacer);
      println("Validation : " + results)
    }
  }

  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  config: Configuration) = {

    val (initLexicon,initProductions) = GenerativeParser.extractCounts(trainTrees.iterator.map(tuple => (tuple._1,tuple._2)));
    val numStates = config.readIn[Int]("discrim.numStates",2);

    val xbarParser = {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initProductions));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val latentFeaturizer = StochasticLatentTrainer.getFeaturizer(config, initLexicon, initProductions, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.activeKeys.map(_._1) if initLexicon(t).size > 50) yield t;
    }

    val threshold = config.readIn("discrim.filterThreshold",-8.0)

    val thresholdingTrainTrees = trainTrees.toIndexedSeq.par(1000).map { case (t,w,s) => (t,w,new ThresholdingScorer(s,threshold))};

    obj = new LatentDiscrimObjective(latentFeaturizer, "",
      thresholdingTrainTrees,
      xbarParser,
      openTags,
      split(_:String,numStates),
      unsplit (_:(String,Int)));


    val iterationsPerEval = config.readIn("iterations.eval",25);
    val maxIterations = config.readIn("iterations.max",300);
    val regularization = config.readIn("objective.regularization",0.01);
    val iterPerValidate = config.readIn("iterations.validate",10);
    val opt = new LBFGS[Int,DenseVector](iterationsPerEval,5) with ConsoleLogging;

    val init = obj.initialWeightVector;

    val log = Log.globalLog;
    val reg = DiffFunction.withL2Regularization(obj, regularization);
    val cachedObj = new CachedDiffFunction(reg);
    for( (state,iter) <- opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex;
         () = quickEval(devTrees,state.x, iter, iterPerValidate)
         if iter != 0 && iter % iterationsPerEval == 0) yield {
      val parser = obj.extractViterbiParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    }

  }
}

object StochasticLatentTrainer extends ParserTrainer {
  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i);
  }

  def unsplit(x: (String,Int)) = x._1;

  override def loadTrainSpans(config: Configuration):Iterable[SpanScorer] = {
    val spanDir = config.readIn[File]("spans.labeled",null);
    if(spanDir eq null) super.loadTrainSpans(config);
    else {
      val spanFile = new File(spanDir,ProjectTreebankToLabeledSpans.TRAIN_SPANS_NAME)
      ProjectTreebankToLabeledSpans.loadSpansFile(spanFile);
    }
  }
  override def loadDevSpans(config: Configuration):Iterable[SpanScorer] = {
    val spanDir = config.readIn[File]("spans.labeled",null);
    if(spanDir eq null) super.loadDevSpans(config);
    else {
      val spanFile = new File(spanDir,ProjectTreebankToLabeledSpans.DEV_SPANS_NAME)
      ProjectTreebankToLabeledSpans.loadSpansFile(spanFile);
    }
  }

  override def loadTestSpans(config: Configuration):Iterable[SpanScorer] = {
    val spanDir = config.readIn[File]("spans.labeled",null);
    if(spanDir eq null) super.loadTestSpans(config);
    else {
      val spanFile = new File(spanDir,ProjectTreebankToLabeledSpans.TEST_SPANS_NAME)
      ProjectTreebankToLabeledSpans.loadSpansFile(spanFile).map(obj.projectCoarseScorer _);
    }
  }

  def loadCoarseIndex(config: Configuration, index: Index[String]):Index[String] = {
    val spanDir = config.readIn[File]("spans.labeled",null);
    if(spanDir eq null) index
    else {
      val spanFile = new File(spanDir,ProjectTreebankToLabeledSpans.SPAN_INDEX_NAME)
      ProjectTreebankToLabeledSpans.loadSpanIndex(spanFile)
    }
  }

  def loadParser(config: Configuration) = {
    val spanDir = config.readIn[File]("parser.base",null);
    if(spanDir eq null) None
    else {
      Some(ProjectTreebankToLabeledSpans.loadParser(spanDir).builder.withCharts(ParseChart.logProb))
    }
  }

  var obj: LatentDiscrimObjective[String,(String,Int),String] = null;

  def quickEval(devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)], weights: DenseVector, iter: Int, iterPerValidate:Int) {
    if(iter % iterPerValidate == 0) {
      ProjectTreebankToLabeledSpans.writeObject((weights,obj.indexedFeatures.decode(weights)),new java.io.File("weights-" +iter +".ser"));
      println("Validating...");
      val parser = obj.extractViterbiParser(weights);
      val fixedTrees = devTrees.take(400).map { case (a,b,c) => (a,b,obj.projectCoarseScorer(c))}.toIndexedSeq;
      val results = ParseEval.evaluate(fixedTrees, parser, unaryReplacer);
      println("Validation : " + results)
    }
  }

  def readObject[T](loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[T]
    oin.close();
    parser;
  }

  def getFeaturizer(config: Configuration,
                    initLexicon: PairedDoubleCounter[String, String],
                    initProductions: PairedDoubleCounter[String, Rule[String]],
                    numStates: Int): Featurizer[(String, Int), String] = {
    val factory = config.readIn[FeaturizerFactory[String, String]]("discrim.featurizerFactory", new PlainFeaturizerFactory[String]);
    val featurizer = factory.getFeaturizer(config, initLexicon, initProductions);
    val latentFactory = config.readIn[LatentFeaturizerFactory]("discrim.latentFactory", new SlavLatentFeaturizerFactory());
    val latentFeaturizer = latentFactory.getFeaturizer(featurizer, numStates);
    val weightsPath = config.readIn[File]("discrim.oldweights",null);
    if(weightsPath == null) {
      latentFeaturizer
    } else {
      println("Using awesome weights...");
      val weights = readObject[(DenseVector,DoubleCounter[Feature[(String,Int),String]])](weightsPath)._2;
      val splitStates = config.readIn[Boolean]("discrim.splitOldWeights",false);
      new CachedWeightsFeaturizer(latentFeaturizer, weights, if(splitStates) FeatureProjectors.split _ else identity _)
    }
  }

  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  config: Configuration) = {

    val (initLexicon,initProductions) = GenerativeParser.extractCounts(trainTrees.iterator.map(tuple => (tuple._1,tuple._2)));
    val numStates = config.readIn[Int]("discrim.numStates",2);

    val xbarParser = loadParser(config) getOrElse {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initProductions));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val latentFeaturizer: Featurizer[(String, Int), String] = getFeaturizer(config, initLexicon, initProductions, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.activeKeys.map(_._1) if initLexicon(t).size > 50) yield t;
    }

    val threshold = config.readIn("discrim.filterThreshold",-8.0)

    val thresholdingTrainTrees = trainTrees.toIndexedSeq.par(1000).map { case (t,w,s) => (t,w,new ThresholdingScorer(s,threshold))};

    obj = new LatentDiscrimObjective(latentFeaturizer, "",
      thresholdingTrainTrees,
      xbarParser,
      openTags,
      split(_:String,numStates),
      unsplit (_:(String,Int)));


    val iterationsPerEval = config.readIn("iterations.eval",25);
    val maxIterations = config.readIn("iterations.max",300);
    val batchSize = config.readIn("opt.batchsize",1000);
    val regularization = config.readIn("objective.regularization",0.01) * batchSize / trainTrees.length;
    val alpha = config.readIn("opt.stepsize",20.0);
    val useL1 = config.readIn("opt.useL1",false);
    System.out.println("UseL1: " + useL1);
    val opt = if(!useL1) {
      new StochasticGradientDescent[Int,DenseVector](alpha,maxIterations,batchSize)
              with AdaptiveGradientDescent.L2Regularization[Int,DenseVector]
              with ConsoleLogging {
        override val lambda = regularization;
      }
    } else {
      new StochasticGradientDescent[Int,DenseVector](alpha,maxIterations,batchSize)
              with AdaptiveGradientDescent.L1Regularization[Int,DenseVector]
              with ConsoleLogging {
        override val lambda = regularization;
      }
    }

    val init = obj.initialWeightVector;
    val iterPerValidate = config.readIn("iterations.validate",10);

    val log = Log.globalLog;
    for( (state,iter) <- opt.iterations(obj,init).take(maxIterations).zipWithIndex;
         () = quickEval(devTrees,state.x, iter, iterPerValidate)
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractViterbiParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }


  }
}