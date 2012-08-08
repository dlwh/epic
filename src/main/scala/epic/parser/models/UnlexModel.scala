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
import projections.GrammarRefinements
import epic.framework.Feature
import java.io.File
import features._
import breeze.linalg._
import epic.trees._
import epic.trees.annotations.{KMAnnotator, TreeAnnotator}
import features.IndicatorFeature
import epic.trees.TreeInstance

@SerialVersionUID(1L)
class UnlexModel[L, L2, W](featurizer: Featurizer[L2, W],
                        ann: TreeAnnotator[L, W, L2],
                        val projections: GrammarRefinements[L, L2],
                        baseFactory: CoreGrammar[L, W],
                        grammar: BaseGrammar[L],
                        lexicon: Lexicon[L, W],
                        initialFeatureVal: (Feature => Option[Double]) = {
                          _ => None
                        }) extends ParserModel[L, W] with Serializable {
  type Inference = DiscParserInference[L, W]

  val indexedFeatures: IndexedFeaturizer[L, L2, W] = IndexedFeaturizer(grammar, lexicon, featurizer, projections)

  def featureIndex = indexedFeatures.index

  override def initialValueForFeature(f: Feature) = initialFeatureVal(f) getOrElse 0.0

  def emptyCounts = new ExpectedCounts(featureIndex)

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(weights, indexedFeatures)
    val grammar = FeaturizedGrammar(this.grammar, this.lexicon, projections, weights, indexedFeatures, lexicon)
    def reannotate(tree: BinarizedTree[L], words: Seq[W]) = {
      val annotated = ann(tree, words)

      val localized = annotated.map { l =>
          projections.labels.project(l) -> projections.labels.localize(l)
      }

      localized
    }

    new DiscParserInference(indexedFeatures, reannotate, grammar, baseFactory)
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    (ecounts.loss, ecounts.counts)
  }

}

case class UnlexModelFactory(baseParser: ParserParams.BaseParser,
                          constraints: ParserParams.Constraints[AnnotatedLabel, String],
                          annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = KMAnnotator(),
                          oldWeights: File = null) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = UnlexModel[AnnotatedLabel, AnnotatedLabel, String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]): MyModel = {
    val transformed = trainTrees.par.map {
      ti =>
        val t = annotator(ti.tree, ti.words)
        TreeInstance(ti.id, t, ti.words)
    }.seq.toIndexedSeq

    val (initLexicon, initBinaries, initUnaries) = this.extractBasicCounts(transformed)

    val (grammar, lexicon) = baseParser.xbarGrammar(trainTrees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, initBinaries, initUnaries)
    val indexedRefinements = GrammarRefinements(grammar, refGrammar, {
      (_: AnnotatedLabel).baseAnnotatedLabel
    })

    val (xbarWords, xbarBinaries, xbarUnaries) = this.extractBasicCounts(trainTrees.map(_.mapLabels(_.baseAnnotatedLabel)))
    val baseFactory = RefinedGrammar.generative(grammar, lexicon, xbarBinaries, xbarUnaries, xbarWords)
    val cFactory = constraints.cachedFactory(AugmentedGrammar.fromRefined(baseFactory))

    val gen = new WordShapeFeaturizer(sum(initLexicon, Axis._0))
    def labelFlattener(l: AnnotatedLabel) = {
      val basic = Seq(l)
      basic map { IndicatorFeature(_) }
    }
    def ruleFlattener(r: Rule[AnnotatedLabel]) = IndexedSeq(r).map(IndicatorFeature)
    val feat = new GenFeaturizer[AnnotatedLabel, String](gen, labelFlattener _, ruleFlattener _)

    val featureCounter = readWeights(oldWeights)
    new UnlexModel[AnnotatedLabel, AnnotatedLabel, String](feat, annotator, indexedRefinements, cFactory, grammar, lexicon, {
      featureCounter.get(_)
    })
  }

}