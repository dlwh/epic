package scalanlp.parser
package models

import features._
import projections.GrammarRefinements
import scalala.tensor.dense.DenseVector
import scalala.library.Library
import java.io.File
import io.Source
import scalala.tensor.Counter
import scalanlp.parser.DerivationScorer.Factory
import scalanlp.epic.Feature
import scalanlp.trees.{TreeInstance, BinarizedTree, AnnotatedLabel}
import scalanlp.trees.annotations.{FilterAnnotations, TreeAnnotator}

class LatentParserModel[L, L3, W](featurizer: Featurizer[L3, W],
                                  reannotate: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                                  val projections: GrammarRefinements[L, L3],
                                  baseFactory: DerivationScorer.Factory[L, W],
                                  grammar: Grammar[L],
                                  lexicon: Lexicon[L, W],
                                  initialFeatureVal: (Feature => Option[Double]) = {
                                    _ => None
                                  }) extends ParserModel[L, W] {
  type L2 = L3
  type Inference = LatentParserInference[L, L2, W]

  val indexedFeatures: FeatureIndexer[L, L2, W] = FeatureIndexer(grammar, lexicon, featurizer, projections)

  def featureIndex = indexedFeatures.index

  override def initialValueForFeature(f: Feature) = {
    initialFeatureVal(f) getOrElse (math.random * 1E-4)
  }

  def emptyCounts = new scalanlp.parser.ExpectedCounts(featureIndex)

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(weights, indexedFeatures)
    val grammar: Factory[L, W] = FeaturizedGrammar(this.grammar, this.lexicon, projections, weights, indexedFeatures, lexicon)

    new LatentParserInference(indexedFeatures, reannotate, grammar, baseFactory, projections)
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    (ecounts.loss, ecounts.counts)
  }

}

case class LatentParserInference[L, L2, W](featurizer: DerivationFeaturizer[L, W, Feature],
                                           reannotate: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                                           grammar: DerivationScorer.Factory[L, W],
                                           baseMeasure: DerivationScorer.Factory[L, W],
                                           projections: GrammarRefinements[L, L2]) extends ParserInference[L, W] {

  // E[T-z|T, params]
  def goldCounts(ti: TreeInstance[L, W], augment: DerivationScorer[L, W]) = {
    val reannotated = reannotate(ti.tree, ti.words)
    val product = grammar.specialize(ti.words) * augment
    val ecounts = LatentTreeMarginal(product, projections.labels, reannotated).expectedCounts(featurizer)

    ecounts
  }

}

case class LatentParserModelFactory(baseParser: ParserParams.BaseParser,
                                    constraints: ParserParams.Constraints[AnnotatedLabel, String],
                                    annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                                    substates: File = null,
                                    numStates: Int = 2,
                                    oldWeights: File = null,
                                    splitFactor: Int = 1) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = LatentParserModel[AnnotatedLabel, (AnnotatedLabel, Int), String]

  def split(x: AnnotatedLabel, counts: Map[AnnotatedLabel, Int], numStates: Int) = {
    for (i <- 0 until counts.getOrElse(x, numStates)) yield (x, i)
  }

  def unsplit(x: (AnnotatedLabel, Int)) = x._1

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)


    val (xbarParser, xbarLexicon) = baseParser.xbarGrammar(trainTrees)

    val baseFactory = DerivationScorerFactory.generative(xbarParser, xbarLexicon, annBinaries, annUnaries, annWords)
    val cFactory = constraints.cachedFactory(baseFactory)

    val substateMap = if (substates != null && substates.exists) {
      val in = Source.fromFile(substates).getLines()
      val pairs = for (line <- in) yield {
        val split = line.split("\\s+")
        AnnotatedLabel(split(0)) -> split(1).toInt
      }
      pairs.toMap + (xbarParser.root -> 1)
    } else {
      Map(xbarParser.root -> 1)
    }

    val gen = new WordShapeFeaturizer(Library.sum(annWords))
    def labelFlattener(l: (AnnotatedLabel, Int)) = {
      val basic = Seq(l)
      basic map (IndicatorFeature)
    }
    val feat = new GenFeaturizer[(AnnotatedLabel, Int), String](gen, labelFlattener _)

    val annGrammar: Grammar[AnnotatedLabel] = Grammar(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(xbarParser, annGrammar, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annGrammar, split(_: AnnotatedLabel, substateMap, numStates), unsplit)
    val finalRefinements = firstLevelRefinements compose secondLevel
    println(finalRefinements.labels)

    val featureCounter = if (oldWeights ne null) {
      val baseCounter = scalanlp.util.readObject[Counter[Feature, Double]](oldWeights)
      baseCounter
    } else {
      Counter[Feature, Double]()
    }

    new LatentParserModel[AnnotatedLabel, (AnnotatedLabel, Int), String](feat,
    annotator,
    finalRefinements,
    cFactory,
    xbarParser,
    xbarLexicon, {
      featureCounter.get(_)
    })
  }
}

