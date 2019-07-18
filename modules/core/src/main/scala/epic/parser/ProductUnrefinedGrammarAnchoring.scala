package epic.parser

import epic.constraints.ChartConstraints

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
 * Creates a product of two [[epic.parser.UnrefinedGrammarAnchoring]]s (product is in log space, so actually a sum.)
 *
 * @author dlwh
 */

final case class ProductUnrefinedGrammarAnchoring[L, W](s1: UnrefinedGrammarAnchoring[L, W],
                                            s2: UnrefinedGrammarAnchoring[L, W],
                                            alpha: Double = 1.0) extends UnrefinedGrammarAnchoring[L, W] {

  //  def sparsityPattern = ChartConstraints.noSparsity[L]
  override def addConstraints(cs: ChartConstraints[L]): UnrefinedGrammarAnchoring[L, W] = copy(s1.addConstraints(cs))

  override val sparsityPattern: ChartConstraints[L] = s1.sparsityPattern & s2.sparsityPattern

  val topology = s1.topology

  def lexicon = s1.lexicon

  def words = s1.words

  // override val sparsityPattern: ChartConstraints[L] = s1.sparsityPattern & s2.sparsityPattern
  // def addConstraints(cs: ChartConstraints[L]): CoreAnchoring[L, W] = new ProductCoreAnchoring(s1.addConstraints(cs), s2, alpha)

  def scoreSpan(begin: Int, end: Int, label: Int) = {
    val r1 = s1.scoreSpan(begin, end, label)
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreSpan(begin, end, label)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val r1 = s1.scoreBinaryRule(begin, split, end, rule)
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreBinaryRule(begin, split, end, rule)
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val r1 = s1.scoreUnaryRule(begin, end, rule)
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreUnaryRule(begin, end, rule)
  }

}


