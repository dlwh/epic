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
import epic.parser.projections.GrammarRefinements
import epic.framework.Feature
import java.io.File
import features._
import breeze.linalg._
import epic.trees._
import epic.trees.annotations.TreeAnnotator
import breeze.config.Help
import epic.features._
import epic.lexicon.Lexicon
import epic.util.CacheBroker
import epic.trees.BinaryRule
import epic.trees.UnaryRule
import epic.trees.TreeInstance
import epic.trees.annotations.KMAnnotator

/**
 * Model for structural annotations, a la Klein and Manning 2003.
 * @param indexedFeatures
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
                        baseFactory: CoreGrammar[L, W],
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

    new AnnotatedParserInference(indexedFeatures, reannotate, grammar, baseFactory)
  }


  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double) {
    m.expectedCounts(indexedFeatures, accum, scale)
  }
}

case class StructModelFactory(baseParser: ParserParams.XbarGrammar,
                              @Help(text= "The kind of annotation to do on the refined grammar. Defaults to ~KM2003")
                              annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = KMAnnotator(),
                              @Help(text="Old weights to initialize with. Optional")
                              oldWeights: File = null) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = StructModel[AnnotatedLabel, AnnotatedLabel, String]


  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: CoreGrammar[AnnotatedLabel, String])(implicit broker: CacheBroker) = {
    val transformed = trainTrees.par.map(annotator).seq.toIndexedSeq

    val (initLexicon, initBinaries, initUnaries) = this.extractBasicCounts(transformed)

    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(trainTrees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, initBinaries, initUnaries)
    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, {
      (_: AnnotatedLabel).baseAnnotatedLabel
    })

    val cFactory = constrainer

    val surfaceFeaturizer = {
      val dsl = new WordFeaturizer.DSL(initLexicon)
      import dsl._

      (
        unigrams(word + clss, 1)
          + suffixes()
          + prefixes()
          + props
        )
    }
    val wordFeaturizer = IndexedWordFeaturizer.fromData(surfaceFeaturizer, transformed.map{_.words})
    def labelFlattener(l: AnnotatedLabel): Seq[AnnotatedLabel] = {
      val basic = Seq(l, l.baseAnnotatedLabel, l.clearFeatures)
      basic
    }

    def selectOneFeature(r: Rule[AnnotatedLabel]):IndexedSeq[Rule[AnnotatedLabel]] = {
      r match {
        case r@BinaryRule(a,b,c) =>
          val base = r.map(_.baseAnnotatedLabel)
          (( for(af <- a.features.iterator) yield base.copy(parent = base.parent.annotate(af))) ++
            ( for(af <- b.features.iterator) yield base.copy(left = base.left.annotate(af))) ++
            ( for(af <- c.features.iterator) yield base.copy(right = base.right.annotate(af)))).toIndexedSeq
        case r@UnaryRule(a,b,chain) =>
          val base = r.map(_.baseAnnotatedLabel)
          (( for(af <- a.features.iterator) yield base.copy(parent = base.parent.annotate(af))) ++
            ( for(af <- b.features.iterator) yield base.copy(child = base.child.annotate(af)))).toIndexedSeq
      }
    }

    def ruleFlattener(r: Rule[AnnotatedLabel]) = {
      IndexedSeq(r, r.map(_.clearFeatures), r.map(_.baseAnnotatedLabel)) ++ selectOneFeature(r)
    }
    val feat = new GenFeaturizer[AnnotatedLabel, String](wordFeaturizer, labelFlattener, ruleFlattener)

    val featureCounter = readWeights(oldWeights)
    val indexedFeaturizer = IndexedFeaturizer(xbarGrammar, xbarLexicon, trainTrees, feat, indexedRefinements)

    new StructModel[AnnotatedLabel, AnnotatedLabel, String](indexedFeaturizer,
    annotator,
    indexedRefinements,
    cFactory,
    xbarGrammar,
    xbarLexicon,
    featureCounter.get)
  }

}