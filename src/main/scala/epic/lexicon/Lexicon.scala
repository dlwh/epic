package epic.lexicon

import breeze.util.Index
import epic.trees.LexicalProduction
import epic.constraints.TagConstraints

/**
 *
 * @author dlwh
 */
trait Lexicon[L, W] {
  def labelIndex: Index[L]
  def anchor(w: IndexedSeq[W]):Localization

  def knownLexicalProductions : TraversableOnce[LexicalProduction[L, W]]

  // TODO, should i make TagConstraints be a case class instead of an interface?
  trait Localization extends TagConstraints[L] {
    def length: Int
    def allowedTags(pos: Int):Set[Int]
  }

}
