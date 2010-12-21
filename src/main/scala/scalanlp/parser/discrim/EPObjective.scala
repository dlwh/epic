package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.parser.projections._
import scalanlp.optimize._

import scalanlp.trees._
import scalanlp.util._
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
class EPObjective[L,L2,W](featurizers: Seq[Featurizer[L2,W]],
                          trees: IndexedSeq[(BinarizedTree[L],Seq[W],SpanScorer)],
                          indexedProjections: ProjectionIndexer[L,L2],
                          coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                          openTags: Set[L2],
                          closedWords: Set[W],
                          numModels: Int,
                          maxEPIterations: Int = 1) 
                        extends AbstractDiscriminativeObjective[L,L2,W](trees,indexedProjections,openTags,closedWords) {

  val root = {
    val splits = indexedProjections.refinementsOf(coarseParser.root)
    require(splits.length == 1)
    splits(0)
  }

  val indexedFeatures: Seq[FeatureIndexer[L2,W]] = featurizers.map { featurizer =>
    val initGrammar = coarseParser.grammar;
    val initLex = coarseParser.lexicon;
    FeatureIndexer[L,L2,W](featurizer, initGrammar, initLex, indexedProjections);
  }

  private val offsets = Array.fill(numModels+1)(0);
  {
    var acc = 0;
    for(m <- 0 to numModels) {
      offsets(m) = acc;
      if(m < numModels)
        acc += indexedFeatures(m).index.size;
    }
  }
  def totalModelSize = offsets.last;

  def extractParser(weights: DenseVector) = {
    val parsers = Array.tabulate(numModels)(extractLogProbBuilder(weights,_));
    val epBuilder = new EPParser[L,L2,W](parsers,coarseParser, Array.fill(numModels)(indexedProjections),maxEPIterations);
    epBuilder;
  }

  protected type Builder = EPParser[L,L2,W]
  protected type Counts = Seq[ExpectedCounts[W]]

  def builder(weights: DenseVector) = extractParser(weights);

  protected def emptyCounts(b: Builder) = b.parsers.map { p => new ExpectedCounts[W](p.grammar)}
  protected def expectedCounts(epBuilder: Builder, t: BinarizedTree[L], w: Seq[W], scorer:SpanScorer) = {
    val charts = epBuilder.buildAllCharts(w,scorer,t);

    import epBuilder._;

    val expectedCounts = for( (p,ParsedSentenceData(inside,outside,z,f0)) <- epBuilder.parsers zip charts) yield {
      val treeCounts = treeToExpectedCounts(p.grammar,p.lexicon,t,w, f0)
      val wordCounts = wordsToExpectedCounts(p,w,inside,outside, z, f0);
      treeCounts -= wordCounts;
    }

    expectedCounts
  }

  def sumCounts(c1: Counts, c2: Counts) = {
    Array.tabulate(numModels)(m => c1(m) += c2(m));
  }

  def countsToObjective(c: Counts) = {
    val weightVectors = for { (e,f) <- c zip indexedFeatures} yield expectedCountsToFeatureVector(f,e);
    val grad = -tileWeightVectors( weightVectors.toArray) value;

    val logProb = c.map(_.logProb);

    println((norm(grad,2), logProb.mkString("(",",",")")));
    assert(grad.forall(!_._2.isInfinite), "wtf grad");
    (-logProb.last,  grad);
  }


  private type LogProbBuilder = CKYChartBuilder[LogProbabilityParseChart,L2,W]
  private def extractLogProbBuilder(weights: DenseVector, model: Int)= {
    val grammar = weightsToGrammar(indexedFeatures(model), projectWeights(weights,model));
    val lexicon = weightsToLexicon(indexedFeatures(model), projectWeights(weights,model));
    val parser = new LogProbBuilder(root, lexicon, grammar, ParseChart.logProb);
    parser
  }


  private def wordsToExpectedCounts(parser: LogProbBuilder, words: Seq[W],
                                    inside: LogProbabilityParseChart[L2],
                                    outside: LogProbabilityParseChart[L2],
                                    totalProb: Double,
                                    spanScorer: SpanScorer) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words, inside, outside, totalProb, spanScorer);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  private def treeToExpectedCounts(g: Grammar[L2],
                                   lexicon: Lexicon[L2,W],
                                   t: BinarizedTree[L],
                                   words: Seq[W],
                                   spanScorer: SpanScorer = SpanScorer.identity):ExpectedCounts[W] = {
    StateSplitting.expectedCounts(g,lexicon,t.map(indexedProjections.refinementsOf _),words,spanScorer);
  }

  def projectWeights(weights: DenseVector, modelIndex: Int) = {
    val result = indexedFeatures(modelIndex).mkDenseVector(0.0);
    for(i <- 0 until result.size) {
      result(i) = weights(i + offsets(modelIndex));
    }
    result;
  }

  def partitionWeights(weights: DenseVector) = {
    Array.tabulate(numModels)(m => projectWeights(weights, m));
  }

  def tileWeightVectors(modelWeights: Array[DenseVector]) = {
    val weights = new DenseVector(totalModelSize);
    var i = 0;
    for(w <- modelWeights) {
      var mi = 0;
      while(mi < w.size) {
        weights(i) = w(mi);
        i += 1;
        mi += 1;
      }
    }
    weights;
  }


  def initialWeightVector = {
    val result = new DenseVector(totalModelSize);
    var m = 0;
    for(f <- 0 until result.size) {
      if(f >= offsets(m+1)) {
        m += 1;
      }
      result(f) = indexedFeatures(m).initialValueFor(f-offsets(m));
    }
    result;
  }


}

object EPTrainer extends ParserTrainer {
  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i);
  }

  def unsplit(x: (String,Int)) = x._1;

  def loadParser(config: Configuration) = {
    val spanDir = config.readIn[File]("parser.base",null);
    if(spanDir eq null) None
    else {
      Some(ProjectTreebankToLabeledSpans.loadParser(spanDir).builder.withCharts(ParseChart.logProb))
    }
  }

  var obj: EPObjective[String,(String,Int),String] = null;

  def quickEval(devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)], weights: DenseVector, iter: Int, iterPerValidate:Int) {
    if(iter % iterPerValidate == 0) {
      val modelWeights = obj.partitionWeights(weights).zip(obj.indexedFeatures).map { case (w,ind) => (w,ind.decode(w))};
      ProjectTreebankToLabeledSpans.writeObject(modelWeights,new java.io.File("weights-" +iter +".ser"));
      println("Validating...");
      val parser = obj.extractParser(weights);
      val fixedTrees = devTrees.take(400).toIndexedSeq;
      val results = ParseEval.evaluate(fixedTrees, parser, unaryReplacer);
      println("Validation : " + results)
    }
  }

  def getFeaturizer(config: Configuration,
                    initLexicon: PairedDoubleCounter[String, String],
                    initBinaries: PairedDoubleCounter[String, BinaryRule[String]],
                    initUnaries: PairedDoubleCounter[String, UnaryRule[String]],
                    numStates: Int, numModels: Int): IndexedSeq[Featurizer[(String, Int), String]] = {
    val factory = config.readIn[FeaturizerFactory[String, String]]("discrim.featurizerFactory", new PlainFeaturizerFactory[String]);
    val featurizer = factory.getFeaturizer(config, initLexicon, initBinaries, initUnaries);
    val latentFactory = config.readIn[LatentFeaturizerFactory]("discrim.latentFactory", new SlavLatentFeaturizerFactory());
    val latentFeaturizer = latentFactory.getFeaturizer(featurizer, numStates);
    val weightsPath = config.readIn[File]("discrim.oldweights",null);
    if(weightsPath == null) {
      Array.fill(numModels)(latentFeaturizer)
    } else {
      println("Using awesome weights...");
      val weightSeq = readObject[Array[(DenseVector,DoubleCounter[Feature[(String,Int),String]])]](weightsPath).map(_._2);
      val splitStates = config.readIn[Boolean]("discrim.splitOldWeights",false);
      def identity(x: Feature[(String,Int),String]) = x;
      val proj: Feature[(String,Int),String]=>Feature[(String,Int),String] = if(splitStates) FeatureProjectors.split[String,String] _ else identity _

      Array.tabulate(numModels){ m =>
        if(m < weightSeq.length)
          new CachedWeightsFeaturizer(latentFeaturizer, weightSeq(m), proj)
        else latentFeaturizer;
      }
    }
  }

  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  config: Configuration) = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees.iterator.map(tuple => (tuple._1,tuple._2)));
    val numStates = config.readIn[Int]("discrim.numStates",2);

    val xbarParser = loadParser(config) getOrElse {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initBinaries),LogCounters.logNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val maxEPIterations = config.readIn[Int]("ep.iterations",1);
    val epModels = config.readIn[Int]("ep.models",2);

    val latentFeaturizer = getFeaturizer(config, initLexicon, initBinaries, initUnaries, numStates, epModels)

    val fineLabelIndex = {
      val index = Index[(String,Int)];
      for( l <- xbarParser.grammar.index; l2 <- split(l,numStates)) {
        index.index(l2)
      }
      index;
    }
    val indexedProjections = new ProjectionIndexer(xbarParser.grammar.index, fineLabelIndex, unsplit);

    val openTags = Set.empty ++ {
      for(t <- initLexicon.activeKeys.map(_._1) if initLexicon(t).size > 50; t2 <- split(t, numStates) iterator ) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = DoubleCounter[String]();
      initLexicon.rows.foreach ( wordCounts += _._2 )
      wordCounts.iterator.filter(_._2 > 10).map(_._1);
    }

    obj = new EPObjective(latentFeaturizer, 
      trainTrees.toIndexedSeq,
      indexedProjections,
      xbarParser,
      openTags,
      closedWords,
      epModels,
      maxEPIterations);


    val iterationsPerEval = config.readIn("iterations.eval",25);
    val maxIterations = config.readIn("iterations.max",300);
    val batchSize = config.readIn("opt.batchsize",1000);
    val regularization = config.readIn("objective.regularization",0.01) * batchSize / trainTrees.length;
    val alpha = config.readIn("opt.stepsize",20.0);
    val useL1 = config.readIn("opt.useL1",false);
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
      val parser = obj.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }


  }
}
