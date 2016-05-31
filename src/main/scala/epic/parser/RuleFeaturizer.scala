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
import epic.trees.{Rule, Production, LexicalProduction}


/**
 * A simple Featurizer that just counts lexical and rule productions that are used.
 * @author dlwh
 */
class RuleFeaturizer[L, W](grammar: RuleTopology[L]) extends RefinedFeaturizer[L, W, Rule[L]] {
  val index = grammar.index

  def anchor(w: IndexedSeq[W]) = new Anchoring {
    val words = w

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      Array(rule:Int)
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      Array(rule:Int)
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      Array.empty[Int]
    }
  }

  override def lock = this
}
