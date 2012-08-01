package epic.parser
package models

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import features._
import projections.GrammarRefinements
import breeze.linalg._
import java.io.File
import io.Source
import epic.framework.Feature
import epic.trees.annotations.{FilterAnnotations, TreeAnnotator}
import epic.trees._

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

  val indexedFeatures: IndexedFeaturizer[L, L2, W] = IndexedFeaturizer(grammar, lexicon, featurizer, projections)

  def featureIndex = indexedFeatures.index

  override def initialValueForFeature(f: Feature) = {
    initialFeatureVal(f) getOrElse (math.random * 1E-5)
  }

  def emptyCounts = new epic.parser.ExpectedCounts(featureIndex)

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
    val product = AugmentedAnchoring.fromRefined(grammar.anchor(ti.words))
    val ecounts = LatentTreeMarginal(product, projections.labels, reannotated).expectedCounts(featurizer)

    ecounts
  }

}

case class LatentParserModelFactory(baseParser: ParserParams.BaseParser,
                                    constraints: ParserParams.Constraints[AnnotatedLabel, String],
                                    annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                                    substates: File = null,
                                    numStates: Int = 2,
                                    oldWeights: File = null) extends ParserModelFactory[AnnotatedLabel, String] {
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

    val presplit = xbarParser.labelIndex.map(l => l -> split(l)).toMap

    def unsplit(x: (AnnotatedLabel, Int)): AnnotatedLabel = x._1

    def splitRule[L, L2](r: Rule[L], split: L=>Seq[L2]):Seq[Rule[L2]] = r match {
      case BinaryRule(a, b, c) => for(aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
        // don't allow non-identity rule refinements for identity rewrites
      case UnaryRule(a, b, chain) if a == b => for(aa <- split(a)) yield UnaryRule(aa, aa, chain)
      case UnaryRule(a, b, chain) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb, chain)
    }

    val gen = new WordShapeFeaturizer(sum(annWords, Axis._0))
    def labelFlattener(l: (AnnotatedLabel, Int)) = {
      val basic = Seq(l)
      basic map (IndicatorFeature)
    }
    val feat = new GenFeaturizer[(AnnotatedLabel, Int), String](gen, labelFlattener _)

    val annGrammar: BaseGrammar[AnnotatedLabel] = BaseGrammar(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(xbarParser, annGrammar, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annGrammar, split _, {splitRule(_ :Rule[AnnotatedLabel], presplit)}, unsplit)
    val finalRefinements = firstLevelRefinements compose secondLevel
    println(finalRefinements.labels)

    val featureCounter = readWeights(oldWeights)

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

