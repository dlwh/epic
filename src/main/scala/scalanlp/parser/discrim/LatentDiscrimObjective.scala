package scalanlp.parser
package discrim

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
import scalanlp.stats.distributions.Rand
import scalala.tensor.dense.{DenseVectorCol, DenseVector}

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
                            specificSpans: Seq[SpanScorer[L2]] = Stream.continually(SpanScorer.identity[L2]),
                            weightedBroker: WeightedSpanBrokerFactory[L,W] = WeightedSpanBrokerFactory.identity[L,W]
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
    val initLex = coarseParser.lexicon
    FeatureIndexer[L,L2,W](featurizer, initLex, indexedProjections)
  }
  val numSpanWeights = weightedBroker.numWeights(indexedProjections.labels.fineIndex, indexedProjections.rules.fineIndex)
  println("Num features: " + indexedFeatures.index.size)
  println("Num span features: " + numSpanWeights)

  /**
   * Returns a parser for a set of weights.
   * Weights are linearized
   */
  def extractParser(weights: DenseVector[Double]) = {
    // XXX fit in the span broker
    val parser = new SimpleChartParser[L,L2,W](builder(weights).builder,new MaxConstituentDecoder(indexedProjections),indexedProjections)
    parser
  }


  /**
   * A parser in the max-semiring
   */
  def extractMaxParser(weights: DenseVector[Double]) = {
    val parser = new SimpleChartParser[L,L2,W](builder(weights).builder.withCharts(ParseChart.viterbi),
      new ViterbiDecoder(indexedProjections.labels),indexedProjections)
    parser
  }

  protected case class Builder(builder: CKYChartBuilder[LogProbabilityParseChart,L2,W],
                               broker: BaseWeightedSpanBroker[L,L2,W])
  protected type Counts = (ExpectedCounts[W],DenseVectorCol[Double])

  protected def builder(weights: DenseVector[Double])= {
    val grammarWeights:DenseVectorCol[Double] = weights.asCol(0 until indexedFeatures.index.size) + 0.0
    val grammar = weightsToGrammar(indexedFeatures, grammarWeights)
    val lexicon = weightsToLexicon(indexedFeatures, grammarWeights)
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb)

    val otherWeights = weights.asCol(indexedFeatures.index.size until (indexedFeatures.index.size + numSpanWeights)) + 0.0
    val actualBroker = weightedBroker.broker(grammar.labelIndex, grammar.index, otherWeights)
    Builder(parser,actualBroker)
  }

  protected def emptyCounts(b: Builder) = new ExpectedCounts[W](b.builder.grammar) -> DenseVector.zeros[Double](numSpanWeights)
  protected def expectedCounts(b: Builder, ti: TreeInstance[L,W], specific: SpanScorer[L2]) = {
    val summed = SpanScorer.sum(specific,new ProjectingSpanScorer(indexedProjections, ti.spanScorer))
    val treeCounts = treeToExpectedCounts(b.builder.grammar, b.builder.lexicon, b.broker, ti, summed)
    val wordCounts = wordsToExpectedCounts(ti, b, summed)
    /*
    println(w)
    println(t render w)
    println("tree:" + treeCounts.logProb + " " + treeCounts.decode(b.grammar))
    println("words:" + wordCounts.logProb + " " + wordCounts.decode(b.grammar))
    if(treeCounts.logProb - wordCounts.logProb > 1E-4) error(t.render(w) + " " + treeCounts + " " + wordCounts)
    */
    val r = (treeCounts._1 -= wordCounts._1, treeCounts._2 -= wordCounts._2)
    if(r._1.logProb.isNaN || r._1.logProb.isInfinite) {
      println("Warning: NaN's in ecounts for" + ti.words)
      emptyCounts(b)
    } else {
      r
    }
  }

  def sumCounts(c1: Counts, c2: Counts) = { (c1._1 += c2._1, c1._2 += c2._2) }

  def countsToObjective(c: Counts) = {
    val counts = expectedCountsToFeatureVector(indexedFeatures, c._1)
    val grad = DenseVector.vertcat(counts,c._2)
    val obj = -c._1.logProb
    (obj,-grad)
  }

  def wordsToExpectedCounts(ti: TreeInstance[L,W],
                            parser: Builder,
                            spanScorer: SpanScorer[L2] = SpanScorer.identity):Counts = {
    val composite = SpanScorer.sum(spanScorer,parser.broker.spanForId(ti.id))
    val spanCounts = DenseVector.zeros[Double](numSpanWeights)
    val visitor = parser.broker.ecountsVisitor(ti.id,spanCounts.data)
    val ecounts = new InsideOutside(parser.builder).expectedCounts(ti.words,composite,visitor)
    ecounts -> spanCounts
  }

  // these expected counts are in normal space, not log space.
  def treeToExpectedCounts(g: Grammar[L2],
                           lexicon: Lexicon[L2,W],
                           broker: BaseWeightedSpanBroker[L,L2,W],
                           ti: TreeInstance[L,W],
                           spanScorer: SpanScorer[L2] = SpanScorer.identity) = {
    val composite = SpanScorer.sum(spanScorer,broker.spanForId(ti.id))
    val spanCounts = DenseVector.zeros[Double](numSpanWeights)
    val visitor = broker.ecountsVisitor(ti.id,spanCounts.data)
    val ecounts = StateSplitting.expectedCounts(g,lexicon,ti.tree.map(indexedProjections.labels.refinementsOf _),ti.words,
      composite,spanVisitor=visitor)

    ecounts -> spanCounts

  }

  def initialWeightVector = {
    val result = DenseVector.zeros[Double](indexedFeatures.index.size + numSpanWeights)
    for(f <- 0 until indexedFeatures.index.size) {
      result(f) = indexedFeatures.initialValueFor(f)
    }

    for(f <- indexedFeatures.index.size until result.size) {
      result(f) = -Rand.uniform.get()/10
    }
    result
  }
}

import FirstOrderMinimizer.OptParams

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
                           splitFactor:Int = 1)

trait LatentPipeline extends ParserPipeline {
  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i)
  }

  def unsplit(x: (String,Int)) = x._1

  type MyFeaturizer
  type MyObjective <: AbstractDiscriminativeObjective[String,(String,Int),String]


  type SpecificParams
  protected implicit def specificManifest : Manifest[SpecificParams]

  type Params = LatentParams[SpecificParams]
  protected lazy val paramManifest = { manifest[Params]}

  def getFeaturizer(params: Params,
                    initLexicon: Counter2[String, String, Double],
                    initBinaries: Counter2[String, BinaryRule[String], Double],
                    initUnaries: Counter2[String, UnaryRule[String], Double],
                    numStates: Int): MyFeaturizer

  def mkObjective(params: Params,
                  latentFeaturizer: MyFeaturizer,
                  trainTrees: IndexedSeq[TreeInstance[String,String]],
                  indexedProjections: GrammarProjections[String,(String,Int)],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Int)],
                  closedWords: Set[String]): MyObjective

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees)
    import params._
    println("NumStates: " + params.numStates)

    val xbarParser = parser.optParser.getOrElse {
      println("building a parser from scratch...")
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries))
      val lexicon = new SimpleLexicon(initLexicon)
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb)
    }


    val indexedProjections = GrammarProjections(xbarParser.grammar, split(_:String,numStates), unsplit)

    val latentFeaturizer: MyFeaturizer = getFeaturizer(params, initLexicon, initBinaries, initUnaries, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t, numStates).iterator ) yield t2
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1)
    }

    val obj = mkObjective(params, latentFeaturizer, trainTrees, indexedProjections, xbarParser, openTags, closedWords)

    val init = obj.initialWeightVector + 0.0

    type OptState = FirstOrderMinimizer[DenseVector[Double],BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState,Int) ) {
      val (state,iter) = pair
      val weights = state.x
      if(iter % iterPerValidate == 0) {
        cacheWeights(params, obj,weights, iter)
        println("Validating...")
        val parser = obj.extractParser(weights)
        println(validate(parser))
      }
    }


    val cachedObj = new CachedBatchDiffFunction[DenseVector[Double]](obj)

    for( (state,iter) <- params.opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _)
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e
    }
  }


  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector[Double], iter: Int)
}

object StochasticLatentPipeline extends LatentPipeline {

  type MyFeaturizer = Featurizer[(String,Int),String]
  type MyObjective = LatentDiscrimObjective[String,(String,Int),String]

  class SpecificParams()
  protected def specificManifest = manifest[SpecificParams]

  def getFeaturizer(params: Params,
                    initLexicon: Counter2[String, String, Double],
                    initBinaries: Counter2[String, BinaryRule[String], Double],
                    initUnaries: Counter2[String, UnaryRule[String], Double],
                    numStates: Int): Featurizer[(String, Int), String] = {
    val factory = params.featurizerFactory
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries)
    val latentFactory = params.latentFactory
    val latentFeaturizer = latentFactory.getFeaturizer(featurizer, numStates)
    val weightsPath = params.oldWeights
    println("old weights: " + weightsPath)
    if(weightsPath == null) {
      latentFeaturizer
    } else {
      val weights = readObject[(DenseVector[Double],Counter[Feature[(String,Int),String],Double])](weightsPath)._2
      val splitFactor = params.splitFactor
      new CachedWeightsFeaturizer(latentFeaturizer, weights, FeatureProjectors.split(_,splitFactor))
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
      closedWords) with ConfiguredLogging

    r
  }


  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector[Double], iter: Int) = {
    val name = if(iter % 20 == 0) {
      new File("weights-a.ser")
    } else {
      new File("weights-b.ser")
    }
    writeObject( name, weights -> obj.indexedFeatures.decode(weights))
  }

}

