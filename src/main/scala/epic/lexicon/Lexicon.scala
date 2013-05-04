package epic.lexicon

import breeze.util.Index
import epic.trees.LexicalProduction

/**
 *
 * @author dlwh
 */
trait Lexicon[L, W] {
  def labelIndex: Index[L]
  def anchor(w: IndexedSeq[W]):Localization

  def knownLexicalProductions : TraversableOnce[LexicalProduction[L, W]]

  trait Localization {
    def tagsForWord(pos: Int):Set[Int]
  }

}
