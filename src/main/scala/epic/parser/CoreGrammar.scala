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
trait CoreGrammar[L, W] extends Serializable {
  def grammar: BaseGrammar[L]
  def lexicon: Lexicon[L, W]

  def root = grammar.root
  def index = grammar.index
  def labelIndex = grammar.labelIndex
  def labelEncoder = grammar.labelEncoder

  /**
   * Returns a [[epic.parser.CoreAnchoring]] for this particular sentence.
   * @param words
   * @return
   */
  def anchor(words: IndexedSeq[W]):CoreAnchoring[L, W]

  def asConstraintFactory:ChartConstraints.Factory[L, W] = new Factory[L, W] {
    def constraints(w: IndexedSeq[W]): ChartConstraints[L] = anchor(w).sparsityPattern
  }

  def *(other: CoreGrammar[L, W]):CoreGrammar[L, W] = new CoreGrammar.ProductGrammar(this, other)
}

object CoreGrammar {
  def identity[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]):CoreGrammar[L, W] = {
    val g = grammar
    val l = lexicon
    new CoreGrammar[L, W] {
      def grammar =  g
      def lexicon = l

      def anchor(words: IndexedSeq[W]) = CoreAnchoring.identity(grammar, lexicon, words, ChartConstraints.noSparsity[L])
    }
  }

  case class ProductGrammar[L, W](g1: CoreGrammar[L, W], g2: CoreGrammar[L, W]) extends CoreGrammar[L, W] {
    def grammar: BaseGrammar[L] = g1.grammar
    def lexicon: Lexicon[L, W] = g1.lexicon

    def anchor(words: IndexedSeq[W]): CoreAnchoring[L, W] = g1.anchor(words) * g2.anchor(words)
  }
}