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
import breeze.util.Index
import epic.framework.Feature
import epic.parser.projections.GrammarRefinements
import epic.trees.NullRule


/**
 * A simple Featurizer that just counts lexical and rule productions that are used.
 * @author dlwh
 */
@SerialVersionUID(1L)
class ProductionFeaturizer[L, L2, W](grammar: BaseGrammar[L], refinements: GrammarRefinements[L, L2]) extends RefinedFeaturizer[L, W, Feature] with Serializable {
  val index = {
    val index = Index[Feature]()
    for (r <- refinements.rules.fineIndex) {
      index.index(r)
    }
    for (r <- refinements.labels.fineIndex) {
      index.index(NullRule(r))
    }

    index: Index[Feature]
  }

  def anchor(w: IndexedSeq[W]) = new Anchoring {
    val words = w

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      Array(refinements.rules.globalize(rule, ref))
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      Array(refinements.rules.globalize(rule, ref))
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      Array(refinements.labels.globalize(tag, ref) + refinements.rules.fineIndex.size)
    }
  }
}
