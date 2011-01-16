package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.parser.projections._
import scalanlp.optimize._

import scalanlp.trees._
import scalanlp.config.Configuration
import splitting.StateSplitting
import scalala.tensor.counters.LogCounters
import InsideOutside._
import java.io._
import scalanlp.trees.UnaryChainRemover.ChainReplacer
;
import ParseChart.LogProbabilityParseChart;


import scalala.Scalala._;
import scalala.tensor.counters.Counters._
import scalanlp.util._;

/**
 * 
 * @author dlwh
 */
class LatentDiscrimObjective[L,L2,W](featurizer: Featurizer[L2,W],
                            trees: IndexedSeq[(BinarizedTree[L],Seq[W],SpanScorer[L])],
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
    val parser = new ChartParser[L,L2,W](builder,new ViterbiDecoder(indexedProjections), ProjectingSpanScorer.factory(indexedProjections));
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
  protected def expectedCounts(b: Builder, t: BinarizedTree[L], w: Seq[W], scorer:SpanScorer[L]) = {
    val treeCounts = treeToExpectedCounts(b.grammar,b.lexicon,t,w, scorer);
    val wordCounts = wordsToExpectedCounts(w, b, scorer);
    if(treeCounts.logProb - wordCounts.logProb > 1E-4) error(t.render(w) + " " + treeCounts + " " + wordCounts);
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
                            spanScorer: SpanScorer[L] = SpanScorer.identity) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words, new ProjectingSpanScorer(indexedProjections, spanScorer));
    ecounts
  }

  // these expected counts are in normal space, not log space.
  protected def treeToExpectedCounts(g: Grammar[L2],
                                    lexicon: Lexicon[L2,W],
                                    t: BinarizedTree[L],
                                    words: Seq[W],
                                   spanScorer: SpanScorer[L] = SpanScorer.identity):ExpectedCounts[W] = {
    StateSplitting.expectedCounts(g,lexicon,t.map(indexedProjections.refinementsOf _),words,
      new ProjectingSpanScorer(indexedProjections, spanScorer));
  }

  def initialWeightVector = {
    val result = indexedFeatures.mkDenseVector(0.0);
    for(f <- 0 until result.size) {
      result(f) = indexedFeatures.initialValueFor(f);
    }
    result;
  }
}

trait LatentTrainer extends ParserTrainer {
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

  type MyFeaturizer;
  type MyObjective <: AbstractDiscriminativeObjective[String,(String,Int),String];

  def getFeaturizer(config: Configuration,
                    initLexicon: PairedDoubleCounter[String, String],
                    initBinaries: PairedDoubleCounter[String, BinaryRule[String]],
                    initUnaries: PairedDoubleCounter[String, UnaryRule[String]],
                    numStates: Int): MyFeaturizer;

  def quickEval(obj: AbstractDiscriminativeObjective[String,(String,Int),String],
                indexedProjections: ProjectionIndexer[String,(String,Int)],
                unaryReplacer : ChainReplacer[String],
                devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer[String])], weights: DenseVector) {
    println("Validating...");
    val parser = obj.extractParser(weights);
    val fixedTrees = devTrees.take(400).toIndexedSeq;
    val results = ParseEval.evaluate(fixedTrees, parser, unaryReplacer);
    println("Validation : " + results)
  }


  def mkObjective(conf: Configuration,
                  latentFeaturizer: MyFeaturizer,
                  trainTrees: Seq[(BinarizedTree[String], scala.Seq[String], SpanScorer[String])],
                  indexedProjections: ProjectionIndexer[String, (String, Int)],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Int)],
                  closedWords: Set[String]): MyObjective;

  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer[String])],
                  unaryReplacer : ChainReplacer[String],
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
    val indexedProjections = ProjectionIndexer(xbarParser.grammar.index, fineLabelIndex, unsplit);

    val latentFeaturizer: MyFeaturizer = getFeaturizer(config, initLexicon, initBinaries, initUnaries, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.activeKeys.map(_._1) if initLexicon(t).size > 50; t2 <- split(t, numStates) iterator ) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = DoubleCounter[String]();
      initLexicon.rows.foreach ( wordCounts += _._2 )
      wordCounts.iterator.filter(_._2 > 10).map(_._1);
    }

    val obj = mkObjective(config, latentFeaturizer, trainTrees, indexedProjections, xbarParser, openTags, closedWords)

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
    def evalAndCache(pair: (opt.State,Int) ) {
      val (state,iter) = pair;
      val weights = state.x;
      if(iter % iterPerValidate == 0) {
        cacheWeights(config,obj,weights, iter);
        quickEval(obj,indexedProjections, unaryReplacer, devTrees, weights);
      }
    }

    for( (state,iter) <- opt.iterations(obj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _);
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }
  }


  def cacheWeights(config: Configuration, obj: MyObjective, weights: DenseVector, iter: Int);
}

object StochasticLatentTrainer extends LatentTrainer {

  type MyFeaturizer = Featurizer[(String,Int),String];
  type MyObjective = LatentDiscrimObjective[String,(String,Int),String];

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
      val weights = readObject[(DenseVector,DoubleCounter[Feature[(String,Int),String]])](weightsPath)._2;
      val splitFactor = config.readIn[Int]("discrim.splitFactor",1);
      new CachedWeightsFeaturizer(latentFeaturizer, weights, FeatureProjectors.split(_,splitFactor));
    }
  }

  def mkObjective(config: Configuration,
                  latentFeaturizer: Featurizer[(String, Int), String],
                  trainTrees: Seq[(BinarizedTree[String], scala.Seq[String], SpanScorer[String])],
                  indexedProjections: ProjectionIndexer[String, (String, Int)],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Int)],
                  closedWords: Set[String]) = {
    new LatentDiscrimObjective(latentFeaturizer,
      trainTrees.toIndexedSeq,
      indexedProjections,
      xbarParser,
      openTags,
      closedWords) with ConsoleLogging;
  }


  def cacheWeights(config: Configuration, obj: MyObjective, weights: DenseVector, iter: Int) = {
    writeObject( new File("weights-"+iter +".ser"), weights -> obj.indexedFeatures.decode(weights));
  }

}
