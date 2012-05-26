package scalanlp.parser
package models

import features._
import projections.GrammarRefinements
import scalala.tensor.dense.DenseVector
import scalala.library.Library
import java.io.File
import io.Source
import scalala.tensor.Counter
import scalanlp.epic.Feature
import scalanlp.trees.annotations.{FilterAnnotations, TreeAnnotator}
import scalanlp.trees._

class LatentParserModel[L, L3, W](featurizer: Featurizer[L3, W],
                                  reannotate: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                                  val projections: GrammarRefinements[L, L3],
                                  baseFactory: CoreGrammar[L, W],
                                  grammar: BaseGrammar[L],
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
    val grammar = FeaturizedGrammar(this.grammar, this.lexicon, projections, weights, indexedFeatures, lexicon)

    new LatentParserInference(indexedFeatures, reannotate, grammar, baseFactory, projections)
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    (ecounts.loss, ecounts.counts)
  }

}

case class LatentParserInference[L, L2, W](featurizer: RefinedFeaturizer[L, W, Feature],
                                           reannotate: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                                           grammar: RefinedGrammar[L, W],
                                           baseMeasure: CoreGrammar[L, W],
                                           projections: GrammarRefinements[L, L2]) extends ParserInference[L, W] {

  // E[T-z|T, params]
  def goldCounts(ti: TreeInstance[L, W], augment: CoreAnchoring[L, W]) = {
    val reannotated = reannotate(ti.tree, ti.words)
    val product = AugmentedAnchoring(grammar.specialize(ti.words), augment)
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



  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]):MyModel = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)


    val (xbarParser, xbarLexicon) = baseParser.xbarGrammar(trainTrees)

    val baseFactory = RefinedGrammar.generative(xbarParser, xbarLexicon, annBinaries, annUnaries, annWords)
    val cFactory = constraints.cachedFactory(AugmentedGrammar.fromRefined(baseFactory))

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


    def split(x: AnnotatedLabel): Seq[(AnnotatedLabel, Int)] = {
      for (i <- 0 until substateMap.getOrElse(x, numStates)) yield (x, i)
    }

    def unsplit(x: (AnnotatedLabel, Int)): AnnotatedLabel = x._1

    def splitRule[L, L2](r: Rule[L], split: L=>Seq[L2]):Seq[Rule[L2]] = r match {
      case BinaryRule(a, b, c) => for(aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
        // don't allow ref
      case UnaryRule(a, b) if a == b => for(aa <- split(a)) yield UnaryRule(aa, aa)
      case UnaryRule(a, b) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb)
    }

    val gen = new WordShapeFeaturizer(Library.sum(annWords))
    def labelFlattener(l: (AnnotatedLabel, Int)) = {
      val basic = Seq(l)
      basic map (IndicatorFeature)
    }
    val feat = new GenFeaturizer[(AnnotatedLabel, Int), String](gen, labelFlattener _)

    val annGrammar: BaseGrammar[AnnotatedLabel] = BaseGrammar(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(xbarParser, annGrammar, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annGrammar, split _, {splitRule(_ :Rule[AnnotatedLabel], split _)}, unsplit)
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

