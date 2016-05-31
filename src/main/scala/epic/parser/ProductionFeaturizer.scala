package epic.parser

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
import breeze.util.{Encoder, Index}
import epic.framework.Feature
import epic.parser.projections.GrammarRefinements
import epic.trees.Rule
import epic.features.IndicatorFeature


/**
 * A simple Featurizer that just counts lexical and rule productions that are used.
 * @author dlwh
 */
@SerialVersionUID(1L)
class ProductionFeaturizer[L, L2, W](val topology: RuleTopology[L], refinements: GrammarRefinements[L, L2],
                                     lGen: L2=>Seq[Feature] = {(x:L2)=>if (x.isInstanceOf[Feature]) Seq(x.asInstanceOf[Feature]) else Seq(IndicatorFeature(x))},
                                     rGen: Rule[L2] => Seq[Feature] = {(x: Rule[L2]) => Seq(x)},
                                     filterRedundantFeatures: Boolean = false) extends RefinedFeaturizer[L, W, Feature] with Serializable {

  private val (index_ :Index[Feature], ruleFeatures: Array[Array[Int]], labelFeatures: Array[Array[Int]]) = {
    if (filterRedundantFeatures) {
      val index = epic.features.buildNonRedundantFeatureIndex[Either[Rule[L2], L2], Feature](refinements.rules.fineIndex.iterator.map(Left(_)) ++ refinements.labels.fineIndex.iterator.map(Right(_)), {
        case Left(r) => rGen(r)
        case Right(l) => lGen(l)
      })

      // TODO: I should figure out how to one pass this
      val rules = Encoder.fromIndex(refinements.rules.fineIndex).tabulateArray(r => rGen(r).map(index).toArray.filter(_ != -1))
      val labels = Encoder.fromIndex(refinements.labels.fineIndex).tabulateArray(l => lGen(l).map(index).toArray.filter(_ != -1))
      (index: Index[Feature], rules, labels)
    } else {
      val index = Index[Feature]()
      val rules = Encoder.fromIndex(refinements.rules.fineIndex).tabulateArray(r => rGen(r).map(index.index).toArray)
      val labels = Encoder.fromIndex(refinements.labels.fineIndex).tabulateArray(l => lGen(l).map(index.index).toArray)
      (index: Index[Feature], rules, labels)
    }
  }

  assert(ruleFeatures.forall(_.nonEmpty))
  assert(labelFeatures.forall(_.nonEmpty))

  def index = index_

  def featuresFor(rule: Rule[L2]) = featuresForRule(refinements.rules.fineIndex(rule))
  def featuresFor(label: L2) = featuresForLabel(refinements.labels.fineIndex(label))

  def featuresForRule(r: Int): Array[Int] = ruleFeatures(r)

  def featuresForLabel(l: Int): Array[Int] = labelFeatures(l)

  override def lock: RefinedFeaturizer[L, W, Feature] = this

  def anchor(w: IndexedSeq[W]) = new Anchoring {
    val words = w

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      featuresForRule(refinements.rules.globalize(rule, ref))
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      featuresForRule(refinements.rules.globalize(rule, ref))
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      featuresForLabel(refinements.labels.globalize(tag, ref))
    }
  }
}
