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

/**
 * A class that asks about all anchored spans.
 * It's the "foreach" version of a [[epic.parser.RefinedAnchoring]] that takes in
 * expected counts. score is usually an expected count between 0 and 1.
 * @author dlwh
 */
trait AnchoredVisitor[L] {
  def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double)
  def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double)
  def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double)

  /**
   * Should we bother to visit unary rules?
   * @return
   */
  def skipUnaryRules: Boolean = false
  /**
   * Should we bother to visit binary rules?
   * Visiting is *much* faster if you don't for [[epic.parser.RefinedChartMarginal]]
   * @return
   */
  def skipBinaryRules: Boolean = false
}
