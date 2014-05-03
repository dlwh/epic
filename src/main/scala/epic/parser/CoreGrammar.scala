package epic.parser

import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints
import epic.constraints.ChartConstraints.Factory

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
 * A CoreGrammar is a weighted grammar over x-bar symbols
 * (that is, symbols that are not refined or annotated) that
 * can be "anchored" to a sentence, giving a
 * [[epic.parser.CoreAnchoring]]. This anchoring
 * can be used to parse.
 *
 * @author dlwh
 * @tparam L label type
 * @tparam W word type
 */
trait CoreGrammar[L, W] extends Serializable { core =>
  def topology: RuleTopology[L]
  def lexicon: Lexicon[L, W]

  def root = topology.root
  def index = topology.index
  def labelIndex = topology.labelIndex
  def labelEncoder = topology.labelEncoder

  /**
   * Returns a [[epic.parser.CoreAnchoring]] for this particular sentence.
   * @param words
   * @return
   */
  def anchor(words: IndexedSeq[W]):CoreAnchoring[L, W]

  def *(other: CoreGrammar[L, W]):CoreGrammar[L, W] = new CoreGrammar.ProductGrammar(this, other)


  def lift: Grammar[L, W] = new Grammar[L, W] {
    override def topology: RuleTopology[L] = core.topology

    override def lexicon: Lexicon[L, W] = core.lexicon

    override def anchor(words: IndexedSeq[W], constraints: ChartConstraints[L]): RefinedAnchoring[L, W] = core.anchor(words).lift(constraints)

  }
}

object CoreGrammar {
  def identity[L, W](topology: RuleTopology[L], lexicon: Lexicon[L, W]):CoreGrammar[L, W] = new IdentityCoreGrammar(topology, lexicon)

  @SerialVersionUID(1L)
  case class IdentityCoreGrammar[L, W](topology: RuleTopology[L], lexicon: Lexicon[L, W]) extends CoreGrammar[L, W] {
    def anchor(words: IndexedSeq[W]) = CoreAnchoring.identity(topology, lexicon, words)
  }

  case class ProductGrammar[L, W](g1: CoreGrammar[L, W], g2: CoreGrammar[L, W]) extends CoreGrammar[L, W] {
    def topology: RuleTopology[L] = g1.topology
    def lexicon: Lexicon[L, W] = g1.lexicon

    def anchor(words: IndexedSeq[W]): CoreAnchoring[L, W] = g1.anchor(words) * g2.anchor(words)
  }
}