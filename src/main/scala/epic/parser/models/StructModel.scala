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
import epic.parser.projections.{ConstraintCoreGrammarAdaptor, ParserChartConstraintsFactory, GrammarRefinements}
import epic.framework.Feature
import java.io.File
import features._
import breeze.linalg._
import epic.trees._
import epic.trees.annotations.{KMAnnotator, TreeAnnotator}
import features.IndicatorFeature
import epic.trees.TreeInstance
import breeze.config.Help
import epic.features.{StandardSurfaceFeaturizer, IndexedWordFeaturizer}
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints
import epic.util.CacheBroker
import epic.constraints.ChartConstraints.Factory

/**
 * Model for structural annotations, a la Klein and Manning 2003.
 * @param featurizer
 * @param annotator
 * @param projections
 * @param baseFactory
 * @param baseGrammar
 * @param lexicon
 * @param initialFeatureVal
 * @tparam L
 * @tparam L2
 * @tparam W
 */
@SerialVersionUID(1L)
class StructModel[L, L2, W](indexedFeatures: IndexedFeaturizer[L, L2, W],
                        annotator: TreeAnnotator[L, W, L2],
                        val projections: GrammarRefinements[L, L2],
                        baseFactory: ChartConstraints.Factory[L, W],
                        val baseGrammar: BaseGrammar[L],
                        val lexicon: Lexicon[L, W],
                        initialFeatureVal: (Feature => Option[Double]) = { _ => None }) extends ParserModel[L, W] with Serializable {
  type Inference = AnnotatedParserInference[L, W]

  def featureIndex = indexedFeatures.index

  override def initialValueForFeature(f: Feature) = initialFeatureVal(f) getOrElse 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(weights, indexedFeatures)
    val grammar = FeaturizedGrammar(this.baseGrammar, this.lexicon, projections, weights, indexedFeatures, lexicon)
    def reannotate(tree: BinarizedTree[L], words: Seq[W]) = {
      val annotated = annotator(tree, words)

      val localized = annotated.map { l =>
          projections.labels.project(l) -> projections.labels.localize(l)
      }

      localized
    }

    new AnnotatedParserInference(indexedFeatures, reannotate, grammar, new ConstraintCoreGrammarAdaptor(this.baseGrammar, this.lexicon, baseFactory))
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    (ecounts.loss, ecounts.counts)
  }

}

case class StructModelFactory(baseParser: ParserParams.XbarGrammar,
                              @Help(text= "The kind of annotation to do on the refined grammar. Defaults to ~KM2003")
                              annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = KMAnnotator(),
                              @Help(text="Old weights to initialize with. Optional")
                              oldWeights: File = null) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = StructModel[AnnotatedLabel, AnnotatedLabel, String]


  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: Factory[AnnotatedLabel, String])(implicit broker: CacheBroker) = {
    val transformed = trainTrees.par.map(annotator).seq.toIndexedSeq

    val (initLexicon, initBinaries, initUnaries) = this.extractBasicCounts(transformed)

    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(trainTrees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, initBinaries, initUnaries)
    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, {
      (_: AnnotatedLabel).baseAnnotatedLabel
    })

    val cFactory = constrainer

    val surfaceFeaturizer = new StandardSurfaceFeaturizer(sum(initLexicon, Axis._0))
    val wordFeaturizer = IndexedWordFeaturizer.fromData(surfaceFeaturizer, trainTrees.map{_.words})
    def labelFlattener(l: AnnotatedLabel) = {
      val basic = Seq(l)
      basic map { IndicatorFeature(_) }
    }
    def ruleFlattener(r: Rule[AnnotatedLabel]) = IndexedSeq(r).map(IndicatorFeature)
    val feat = new GenFeaturizer[AnnotatedLabel, String](wordFeaturizer, labelFlattener _, ruleFlattener _)

    val featureCounter = readWeights(oldWeights)
    val indexedFeaturizer = IndexedFeaturizer(xbarGrammar, xbarLexicon, trainTrees, feat, indexedRefinements)

    new StructModel[AnnotatedLabel, AnnotatedLabel, String](indexedFeaturizer,
    annotator,
    indexedRefinements,
    cFactory,
    xbarGrammar,
    xbarLexicon,
    { featureCounter.get(_) })
  }

}