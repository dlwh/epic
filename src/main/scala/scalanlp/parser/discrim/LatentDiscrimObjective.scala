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
                            trees: IndexedSeq[(BinarizedTree[L],Seq[W],SpanScorer)],
                            indexedProjections: ProjectionIndexer[L,L2],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                            openTags: Set[L2],
                            closedWords: Set[W]
                            ) extends AbstractDiscriminativeObjective[L,L2,W](trees,indexedProjections,openTags,closedWords) {


  val root = {
    val splits = indexedProjections.refinementsOf(coarseParser.root)
    require(splits.length == 1)
    splits(0)
  }

  val indexedFeatures: FeatureIndexer[L2,W] = {
    val initGrammar = coarseParser.grammar;
    val initLex = coarseParser.lexicon;
    FeatureIndexer[L,L2,W](featurizer, initGrammar, initLex, indexedProjections);
  }

  def extractParser(weights: DenseVector) = {
    val grammar = weightsToGrammar(indexedFeatures, weights);
    val lexicon = weightsToLexicon(indexedFeatures, weights);
    val builder = CKYChartBuilder[L2,W](root, lexicon, grammar);
    val parser = new ChartParser[L,L2,W](builder,new ViterbiDecoder(indexedProjections), projectCoarseScorer(indexedProjections,_));
    parser
  }

  protected type Builder = CKYChartBuilder[LogProbabilityParseChart,L2,W];
  protected type Counts = ExpectedCounts[W];

  protected def builder(weights: DenseVector)= {
    val grammar = weightsToGrammar(indexedFeatures, weights);
    val lexicon = weightsToLexicon(indexedFeatures, weights);
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb);
    parser
  }
  
  protected def emptyCounts(b: Builder) = new ExpectedCounts[W](b.grammar)
  protected def expectedCounts(b: Builder, t: BinarizedTree[L], w: Seq[W], scorer:SpanScorer) = {
    val treeCounts = treeToExpectedCounts(b.grammar,b.lexicon,t,w, scorer);
    val wordCounts = wordsToExpectedCounts(w, b, scorer);
    treeCounts -= wordCounts;
  }

  def sumCounts(c1: Counts, c2: Counts) = { c1 += c2}

  def countsToObjective(c: Counts) = {
    val grad = -expectedCountsToFeatureVector(indexedFeatures, c) value;
    val obj = -c.logProb
    (obj,grad);
  }

  protected def wordsToExpectedCounts(words: Seq[W],
                            parser: ChartBuilder[LogProbabilityParseChart,L2,W],
                            spanScorer: SpanScorer = SpanScorer.identity) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words, spanScorer);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  protected def treeToExpectedCounts(g: Grammar[L2],
                                    lexicon: Lexicon[L2,W],
                                    t: BinarizedTree[L],
                                    words: Seq[W],
                                   spanScorer: SpanScorer = SpanScorer.identity):ExpectedCounts[W] = {
    StateSplitting.expectedCounts(g,lexicon,t.map(indexedProjections.refinementsOf _),words,spanScorer);
  }

  def initialWeightVector = {
    val result = indexedFeatures.mkDenseVector(0.0);
    for(f <- 0 until result.size) {
      result(f) = indexedFeatures.initialValueFor(f);
    }
    result;
  }


}

object StochasticLatentTrainer extends ParserTrainer {
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

  var obj: LatentDiscrimObjective[String,(String,Int),String] = null;

  def quickEval(indexedProjections: ProjectionIndexer[String,(String,Int)], devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)], weights: DenseVector, iter: Int, iterPerValidate:Int) {
    if(iter % iterPerValidate == 0) {
      ProjectTreebankToLabeledSpans.writeObject((weights,obj.indexedFeatures.decode(weights)),new java.io.File("weights-" +iter +".ser"));
      println("Validating...");
      val parser = obj.extractParser(weights);
      val fixedTrees = devTrees.take(400).map { case (a,b,c) => (a,b,obj.projectCoarseScorer(indexedProjections,c))}.toIndexedSeq;
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
                    initBinaries: PairedDoubleCounter[String, BinaryRule[String]],
                    initUnaries: PairedDoubleCounter[String, UnaryRule[String]],
                    numStates: Int): Featurizer[(String, Int), String] = {
    val factory = config.readIn[FeaturizerFactory[String, String]]("discrim.featurizerFactory", new PlainFeaturizerFactory[String]);
    val featurizer = factory.getFeaturizer(config, initLexicon, initBinaries, initUnaries);
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

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees.iterator.map(tuple => (tuple._1,tuple._2)));
    val numStates = config.readIn[Int]("discrim.numStates",2);

    val xbarParser = loadParser(config) getOrElse {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initBinaries),LogCounters.logNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val fineLabelIndex = {
      val index = Index[(String,Int)];
      for( l <- xbarParser.grammar.index; l2 <- split(l,numStates)) {
        index.index(l2)
      }
      index;
    }
    val indexedProjections = new ProjectionIndexer(xbarParser.grammar.index, fineLabelIndex, unsplit);

    val latentFeaturizer: Featurizer[(String, Int), String] = getFeaturizer(config, initLexicon, initBinaries, initUnaries, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.activeKeys.map(_._1) if initLexicon(t).size > 50; t2 <- split(t, numStates) iterator ) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = DoubleCounter[String]();
      initLexicon.rows.foreach ( wordCounts += _._2 )
      wordCounts.iterator.filter(_._2 > 10).map(_._1);
    }

    obj = new LatentDiscrimObjective(latentFeaturizer, 
      trainTrees.toIndexedSeq,
      indexedProjections,
      xbarParser,
      openTags,
      closedWords)

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
         () = quickEval(indexedProjections,devTrees.toIndexedSeq,state.x, iter, iterPerValidate)
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }


  }
}
