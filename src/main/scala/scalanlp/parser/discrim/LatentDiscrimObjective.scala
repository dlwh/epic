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
import ParseChart.LogProbabilityParseChart


import scalala.Scalala._;
import scalala.tensor.counters.Counters._
import scalanlp.util._;

/**
 * 
 * @author dlwh
 */
class LatentDiscrimObjective[L,L2,W](featurizer: Featurizer[L2,W],
                            trees: IndexedSeq[TreeInstance[L,W]],
                            indexedProjections: ProjectionIndexer[L,L2],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                            openTags: Set[L2],
                            closedWords: Set[W]
                            ) extends AbstractDiscriminativeObjective[L,L2,W](trees,indexedProjections,openTags,closedWords) {


  val root = {
    val splits = indexedProjections.refinementsOf(coarseParser.root)
    require(splits.length == 1, splits)
    splits(0)
  }

  val indexedFeatures: FeatureIndexer[L2,W] = {
    val initGrammar = coarseParser.grammar;
    val initLex = coarseParser.lexicon;
    FeatureIndexer[L,L2,W](featurizer, initGrammar, initLex, indexedProjections);
  }
  println("Num features: " + indexedFeatures.index.size);

  def extractParser(weights: DenseVector) = {
    val parser = new ChartParser[L,L2,W](builder(weights),new MaxConstituentDecoder(indexedProjections),indexedProjections);
    parser
  }


  def extractMaxParser(weights: DenseVector) = {
    val parser = new ChartParser[L,L2,W](builder(weights).withCharts(ParseChart.viterbi),
      new ViterbiDecoder(indexedProjections),indexedProjections);
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
    /*
    println(w);
    println(t render w)
    println("tree:" + treeCounts.logProb + " " + treeCounts.decode(b.grammar));
    println("words:" + wordCounts.logProb + " " + wordCounts.decode(b.grammar));
    if(treeCounts.logProb - wordCounts.logProb > 1E-4) error(t.render(w) + " " + treeCounts + " " + wordCounts);
    */
    treeCounts -= wordCounts
//    println("Acc: " + treeCounts.logProb + " " +  treeCounts.decode(b.grammar) )
//    treeCounts;
  }

  def sumCounts(c1: Counts, c2: Counts) = { c1 += c2}

  def countsToObjective(c: Counts) = {
    val grad = -expectedCountsToFeatureVector(indexedFeatures, c) value;
    val obj = -c.logProb
    (obj,grad);
  }

  def wordsToExpectedCounts(words: Seq[W],
                                      parser: ChartBuilder[LogProbabilityParseChart,L2,W],
                                      spanScorer: SpanScorer[L] = SpanScorer.identity) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words, new ProjectingSpanScorer(indexedProjections, spanScorer));
    ecounts
  }

  // these expected counts are in normal space, not log space.
  def treeToExpectedCounts(g: Grammar[L2],
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

import FirstOrderMinimizer.OptParams;

case class LatentParams[P](parser: ParserParams.BaseParser,
                           opt: OptParams,
                           specific: P,
                           featurizerFactory: FeaturizerFactory[String,String] = new PlainFeaturizerFactory[String],
                           latentFactory: LatentFeaturizerFactory = new SlavLatentFeaturizerFactory(),
                           numStates: Int= 8,
                           iterationsPerEval: Int = 50,
                           maxIterations: Int = 202,
                           iterPerValidate: Int = 10,
                           oldWeights: File = null,
                           splitFactor:Int = 1);

trait LatentTrainer extends ParserTrainer {
  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i);
  }

  def unsplit(x: (String,Int)) = x._1;

  type MyFeaturizer;
  type MyObjective <: AbstractDiscriminativeObjective[String,(String,Int),String];


  type SpecificParams;
  protected implicit def specificManifest : Manifest[SpecificParams];

  type Params = LatentParams[SpecificParams];
  protected lazy val paramManifest = { println(manifest[Params]); manifest[Params]}

  def getFeaturizer(params: Params,
                    initLexicon: PairedDoubleCounter[String, String],
                    initBinaries: PairedDoubleCounter[String, BinaryRule[String]],
                    initUnaries: PairedDoubleCounter[String, UnaryRule[String]],
                    numStates: Int): MyFeaturizer;

  def quickEval(obj: AbstractDiscriminativeObjective[String,(String,Int),String],
                indexedProjections: ProjectionIndexer[String,(String,Int)],
                unaryReplacer : ChainReplacer[String],
                devTrees: Seq[TreeInstance[String,String]], weights: DenseVector) {
    println("Validating...");
    val parser = obj.extractParser(weights);
    val fixedTrees = devTrees.take(400).toIndexedSeq;
    val results = ParseEval.evaluate(fixedTrees, parser, unaryReplacer);
    println("Validation : " + results)
  }


  def mkObjective(params: Params,
                  latentFeaturizer: MyFeaturizer,
                  trainTrees: IndexedSeq[TreeInstance[String,String]],
                  indexedProjections: ProjectionIndexer[String, (String, Int)],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Int)],
                  closedWords: Set[String]): MyObjective;

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees: IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  params: Params) = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);
    import params._;
    println("NumStates: " + params.numStates);

    val xbarParser = parser.optParser.getOrElse {
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

    val latentFeaturizer: MyFeaturizer = getFeaturizer(params, initLexicon, initBinaries, initUnaries, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.activeKeys.map(_._1) if initLexicon(t).size > 50; t2 <- split(t, numStates) iterator ) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = DoubleCounter[String]();
      initLexicon.rows.foreach ( wordCounts += _._2 )
      wordCounts.iterator.filter(_._2 > 5).map(_._1);
    }

    val obj = mkObjective(params, latentFeaturizer, trainTrees, indexedProjections, xbarParser, openTags, closedWords)

    val optimizer = opt.minimizer(obj,trainTrees.length);

    val init = obj.initialWeightVector.copy;

    val log = Log.globalLog;
    import scalanlp.optimize.RandomizedGradientCheckingFunction;
    val rand = new RandomizedGradientCheckingFunction(obj);
    def evalAndCache(pair: (optimizer.State,Int) ) {
      val (state,iter) = pair;
      val weights = state.x;
      if(iter % iterPerValidate == 0) {
        cacheWeights(params, obj,weights, iter);
        quickEval(obj,indexedProjections, unaryReplacer, devTrees, weights);
      }
    }


    val cachedObj = new CachedBatchDiffFunction[Int,DenseVector](obj);

    for( (state,iter) <- optimizer.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _);
//         _ = rand.calculate(state.x);
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }
  }


  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector, iter: Int);
}

object StochasticLatentTrainer extends LatentTrainer {

  type MyFeaturizer = Featurizer[(String,Int),String];
  type MyObjective = LatentDiscrimObjective[String,(String,Int),String];

  class SpecificParams();
  protected def specificManifest = manifest[SpecificParams];

  def getFeaturizer(params: Params,
                    initLexicon: PairedDoubleCounter[String, String],
                    initBinaries: PairedDoubleCounter[String, BinaryRule[String]],
                    initUnaries: PairedDoubleCounter[String, UnaryRule[String]],
                    numStates: Int): Featurizer[(String, Int), String] = {
    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);
    val latentFactory = params.latentFactory;
    val latentFeaturizer = latentFactory.getFeaturizer(featurizer, numStates);
    val weightsPath = params.oldWeights;
    println("old weights: " + weightsPath);
    if(weightsPath == null) {
      latentFeaturizer
    } else {
      val weights = readObject[(DenseVector,DoubleCounter[Feature[(String,Int),String]])](weightsPath)._2;
      val splitFactor = params.splitFactor;
      new CachedWeightsFeaturizer(latentFeaturizer, weights, FeatureProjectors.split(_,splitFactor));
    }
  }

  override def mkObjective(params: Params,
                  latentFeaturizer: Featurizer[(String, Int), String],
                  trainTrees: IndexedSeq[TreeInstance[String,String]],
                  indexedProjections: ProjectionIndexer[String, (String, Int)],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Int)],
                  closedWords: Set[String]) = {
    val r = new LatentDiscrimObjective(latentFeaturizer,
      trainTrees,
      indexedProjections,
      xbarParser,
      openTags,
      closedWords) with ConsoleLogging;

    r
  }


  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector, iter: Int) = {
    writeObject( new File("weights-"+iter +".ser"), weights -> obj.indexedFeatures.decode(weights));
  }

}

