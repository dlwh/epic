package scalanlp.parser.epic

import scalanlp.parser._
import features.{Feature, IndicatorFeature, WordShapeFeaturizer}
import projections.GrammarRefinements
import ParseChart.LogProbabilityParseChart
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.library.Library
import scalanlp.trees._
import java.io.File
import scalala.tensor.Counter
import scalala.tensor.mutable.Counter2

class KMModel[L, L3, W](featurizer: Featurizer[L3, W],
                        ann: (BinarizedTree[L], Seq[W])=>BinarizedTree[L3],
                        val projections: GrammarRefinements[L, L3],
                        grammar: Grammar[L],
                        lexicon: Lexicon[L, W],
                        initialFeatureVal: (Feature=>Option[Double]) = { _ => None}) extends ParserModel[L, W] {
  type L2 = L3
  type Inference = DiscParserInference[L, L2, W]

  val indexedFeatures: FeatureIndexer[L, L2, W]  = FeatureIndexer(grammar, lexicon, featurizer, projections)
  def featureIndex = indexedFeatures.index

  override def initialValueForFeature(f: Feature) = initialFeatureVal(f) getOrElse 0.0

  def emptyCounts = new ExpectedCounts(featureIndex)
  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(weights, indexedFeatures)
    val grammar = FeaturizedGrammar(this.grammar, this.lexicon, projections, weights, indexedFeatures, lexicon)

    new DiscParserInference(indexedFeatures, ann, grammar, projections)
  }

  def extractParser(weights: DenseVector[Double]):ChartParser[L, W] = {
    val inf = inferenceFromWeights(weights)
    val builder = CKYChartBuilder(inf.grammar, ParseChart.logProb)
    new SimpleChartParser(builder, new MaxConstituentDecoder[L, W])
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    (ecounts.loss, ecounts.counts)
  }

}

case class DiscParserInference[L, L2, W](featurizer: DerivationFeaturizer[L, W, Feature],
                                         ann: (BinarizedTree[L], Seq[W])=>BinarizedTree[L2],
                                         grammar: DerivationScorer.Factory[L, W],
                                         projections: GrammarRefinements[L, L2]) extends ParserInference[L, W] {

  // E[T-z|T, params]
  def goldCounts(ti: TreeInstance[L, W], grammar: DerivationScorer[L, W]) = {
    val tree = ti.tree
    val words = ti.words
    val annotated = ann(tree, words)

    val localized = annotated.map { l =>
      projections.labels.project(l) -> projections.labels.localize(l)
    }

    TreeMarginal(this.grammar.grammar, grammar, ti.words, localized).expectedCounts(featurizer)
  }

}

case class KMModelFactory(baseParser: ParserParams.BaseParser,
                          pipeline: KMPipeline,
                          oldWeights: File = null) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = KMModel[AnnotatedLabel, AnnotatedLabel, String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]):MyModel = {
    val transformed = trainTrees.par.map { ti =>
      val t = pipeline(ti.tree, ti.words)
      TreeInstance(ti.id, t, ti.words)
    }.seq.toIndexedSeq

    val (initLexicon, initBinaries, initUnaries) = this.extractBasicCounts(transformed)

    val (grammar,lexicon) = baseParser.xbarGrammar(trainTrees)
    val refGrammar = Grammar(AnnotatedLabel.TOP, initBinaries, initUnaries)
    val indexedRefinements = GrammarRefinements(grammar, refGrammar, {(_:AnnotatedLabel).baseAnnotatedLabel})

    val gen = new WordShapeFeaturizer(Library.sum(initLexicon))
    def labelFlattener(l: AnnotatedLabel) = {
      val basic = Seq(l, l.copy(features=Set.empty))
      basic map {IndicatorFeature(_)}
    }
    val feat = new SumFeaturizer[AnnotatedLabel, String](new RuleFeaturizer(labelFlattener _), new LexFeaturizer(gen, labelFlattener _))
    val xbarLexicon = Counter2[AnnotatedLabel, String, Double]()
    for( (t, w, v) <- initLexicon.triplesIterator) {
      xbarLexicon(t.baseAnnotatedLabel, w) += v
    }

    val featureCounter = if(oldWeights ne null) {
      scalanlp.util.readObject[Counter[Feature, Double]](oldWeights)
    } else {
      Counter[Feature, Double]()
    }
    new KMModel[AnnotatedLabel, AnnotatedLabel, String](feat, pipeline, indexedRefinements, grammar, lexicon, {featureCounter.get(_)})
  }

}

