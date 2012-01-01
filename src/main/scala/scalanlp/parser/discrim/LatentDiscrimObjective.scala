package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.parser.projections._
import scalanlp.optimize._

import scalanlp.trees._
import scalanlp.config.Configuration
import splitting.StateSplitting
import InsideOutside._
import java.io._
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import ParseChart.LogProbabilityParseChart

import scalanlp.util._
import logging._
import scalala.tensor.{Counter, Counter2, ::}
import scalala.library.Library
import scalala.library.Library._

/**
 * This guy does most of the work for the latent parser
 * Objective Function computes suff stats given tree and given words under log-loss (usually)
 * @author dlwh
 */
class LatentDiscrimObjective[L,L2,W](featurizer: Featurizer[L2,W],
                            trees: IndexedSeq[TreeInstance[L,W]],
                            indexedProjections: GrammarProjections[L,L2],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                            openTags: Set[L2],
                            closedWords: Set[W],
                            specificSpans: Seq[SpanScorer[L2]] = Stream.continually(SpanScorer.identity[L2])
                            ) extends AbstractDiscriminativeObjective[L,L2,W](trees,indexedProjections,openTags,closedWords, specificSpans) {


  /** The split root symbol */
  val root = {
    val splits = indexedProjections.labels.refinementsOf(coarseParser.root)
    require(splits.length == 1, splits)
    splits(0)
  }

  /**
   * For each production, its features
   */
  val indexedFeatures: FeatureIndexer[L2,W] = {
    val initLex = coarseParser.lexicon;
    FeatureIndexer[L,L2,W](featurizer, initLex, indexedProjections);
  }
  println("Num features: " + indexedFeatures.index.size);

  /**
   * Returns a parser for a set of weights.
   * Weights are linearized
   */
  def extractParser(weights: DenseVector[Double]) = {
    val parser = new SimpleChartParser[L,L2,W](builder(weights),new MaxConstituentDecoder(indexedProjections),indexedProjections);
    parser
  }


  /**
   * A parser in the max-semiring
   */
  def extractMaxParser(weights: DenseVector[Double]) = {
    val parser = new SimpleChartParser[L,L2,W](builder(weights).withCharts(ParseChart.viterbi),
      new ViterbiDecoder(indexedProjections.labels),indexedProjections);
    parser
  }

  protected type Builder = CKYChartBuilder[LogProbabilityParseChart,L2,W];
  protected type Counts = ExpectedCounts[W];

  protected def builder(weights: DenseVector[Double])= {
    val grammar = weightsToGrammar(indexedFeatures, weights);
    val lexicon = weightsToLexicon(indexedFeatures, weights);
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb);
    parser
  }

  protected def emptyCounts(b: Builder) = new ExpectedCounts[W](b.grammar)
  protected def expectedCounts(b: Builder, t: BinarizedTree[L], w: Seq[W], scorer:SpanScorer[L], specific: SpanScorer[L2]) = {
    val summed = SpanScorer.sum(new ProjectingSpanScorer(indexedProjections, scorer),specific)
    val treeCounts = treeToExpectedCounts(b.grammar,b.lexicon,t,w, summed)
    val wordCounts = wordsToExpectedCounts(w, b, summed)
    /*
    println(w);
    println(t render w)
    println("tree:" + treeCounts.logProb + " " + treeCounts.decode(b.grammar));
    println("words:" + wordCounts.logProb + " " + wordCounts.decode(b.grammar));
    if(treeCounts.logProb - wordCounts.logProb > 1E-4) error(t.render(w) + " " + treeCounts + " " + wordCounts);
    */
    treeCounts -= wordCounts
  }

  def sumCounts(c1: Counts, c2: Counts) = { c1 += c2}

  def countsToObjective(c: Counts) = {
    val grad = -expectedCountsToFeatureVector(indexedFeatures, c);
    val obj = -c.logProb
    (obj,grad);
  }

  def wordsToExpectedCounts(words: Seq[W],
                            parser: ChartBuilder[LogProbabilityParseChart,L2,W],
                            spanScorer: SpanScorer[L2] = SpanScorer.identity) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words,spanScorer);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  def treeToExpectedCounts(g: Grammar[L2],
                           lexicon: Lexicon[L2,W],
                           t: BinarizedTree[L],
                           words: Seq[W],
                           spanScorer: SpanScorer[L2] = SpanScorer.identity):ExpectedCounts[W] = {
    StateSplitting.expectedCounts(g,lexicon,t.map(indexedProjections.labels.refinementsOf _),words,
      spanScorer)
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

case class LatentParams[P](parser: ParserParams.BaseParser[String],
                           opt: OptParams,
                           specific: P,
                           featurizerFactory: FeaturizerFactory[String,String] = new PlainFeaturizerFactory[String],
                           latentFactory: LatentFeaturizerFactory = new SlavLatentFeaturizerFactory(),
                           numStates: Int= 2,
                           iterationsPerEval: Int = 50,
                           maxIterations: Int = 202,
                           iterPerValidate: Int = 10,
                           oldWeights: File = null,
                           splitFactor:Int = 1);

trait LatentPipeline extends ParserPipeline {
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
  protected lazy val paramManifest = { manifest[Params]}

  def getFeaturizer(params: Params,
                    initLexicon: Counter2[String, String, Double],
                    initBinaries: Counter2[String, BinaryRule[String], Double],
                    initUnaries: Counter2[String, UnaryRule[String], Double],
                    numStates: Int): MyFeaturizer;

  def mkObjective(params: Params,
                  latentFeaturizer: MyFeaturizer,
                  trainTrees: IndexedSeq[TreeInstance[String,String]],
                  indexedProjections: GrammarProjections[String,(String,Int)],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Int)],
                  closedWords: Set[String]): MyObjective;

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);
    import params._;
    println("NumStates: " + params.numStates);

    val xbarParser = parser.optParser.getOrElse {
      println("building a parser from scratch...")
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }


    val indexedProjections = GrammarProjections(xbarParser.grammar, split(_:String,numStates), unsplit);

    val latentFeaturizer: MyFeaturizer = getFeaturizer(params, initLexicon, initBinaries, initUnaries, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t, numStates).iterator ) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1);
    }

    val obj = mkObjective(params, latentFeaturizer, trainTrees, indexedProjections, xbarParser, openTags, closedWords)

    val optimizer = opt.minimizer(obj);

    val init = obj.initialWeightVector + 0.0;

    import scalanlp.optimize.RandomizedGradientCheckingFunction;
    val rand = new RandomizedGradientCheckingFunction(obj,1E-4);
    def evalAndCache(pair: (optimizer.State,Int) ) {
      val (state,iter) = pair;
      val weights = state.x;
      if(iter % iterPerValidate == 0) {
        cacheWeights(params, obj,weights, iter);
        println("Validating...");
        val parser = obj.extractParser(weights);
        println(validate(parser))
      }
    }


    val cachedObj = new CachedBatchDiffFunction[DenseVector[Double]](obj);

    for( (state,iter) <- optimizer.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _);
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }
  }


  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector[Double], iter: Int);
}

object StochasticLatentPipeline extends LatentPipeline {

  type MyFeaturizer = Featurizer[(String,Int),String];
  type MyObjective = LatentDiscrimObjective[String,(String,Int),String];

  class SpecificParams();
  protected def specificManifest = manifest[SpecificParams];

  def getFeaturizer(params: Params,
                    initLexicon: Counter2[String, String, Double],
                    initBinaries: Counter2[String, BinaryRule[String], Double],
                    initUnaries: Counter2[String, UnaryRule[String], Double],
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
      val weights = readObject[(DenseVector[Double],Counter[Feature[(String,Int),String],Double])](weightsPath)._2;
      val splitFactor = params.splitFactor;
      new CachedWeightsFeaturizer(latentFeaturizer, weights, FeatureProjectors.split(_,splitFactor));
    }
  }

  override def mkObjective(params: Params,
                  latentFeaturizer: Featurizer[(String, Int), String],
                  trainTrees: IndexedSeq[TreeInstance[String,String]],
                  indexedProjections: GrammarProjections[String,(String,Int)],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Int)],
                  closedWords: Set[String]) = {
    val r = new LatentDiscrimObjective[String,(String,Int),String](latentFeaturizer,
      trainTrees,
      indexedProjections,
      xbarParser,
      openTags,
      closedWords) with ConfiguredLogging;

    r
  }


  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector[Double], iter: Int) = {
    val name = if(iter % 20 == 0) {
      new File("weights-a.ser")
    } else {
      new File("weights-b.ser")
    }
    writeObject( name, weights -> obj.indexedFeatures.decode(weights));
  }

}

