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
import projections.GrammarRefinements
import epic.trees.{LexicalProduction, Production, Rule}
import breeze.util.Index
import epic.framework.Feature

/**
 *
 * @author dlwh
 */

trait RefinedFeaturizer[L, W, Feat]  {

  def index: Index[Feat]

  def forTesting = lock
  
  def anchor(words: IndexedSeq[W]):Anchoring
  
  trait Anchoring {
    def words: IndexedSeq[W]

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int):Array[Int]
    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int):Array[Int]
    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int):Array[Int]
  }

  def lock: RefinedFeaturizer[L, W, Feat]

}
