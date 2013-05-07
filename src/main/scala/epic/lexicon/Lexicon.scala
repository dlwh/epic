package epic.lexicon

import breeze.util.Index
import epic.trees.LexicalProduction
import epic.pruning.TagConstraints

/**
 *
 * @author dlwh
 */
trait Lexicon[L, W] {
  def labelIndex: Index[L]
  def anchor(w: IndexedSeq[W]):Localization

  def knownLexicalProductions : TraversableOnce[LexicalProduction[L, W]]

  // TODO, should i make TagConstraints be a case class instead of an interface?
  trait Localization{
    def tagsForWord(pos: Int):Set[Int]

    def asTagConstraints:TagConstraints[L] = new TagConstraints[L] {
      def allowedTags(pos: Int): Set[Int] = tagsForWord(pos)
    }
  }

}
