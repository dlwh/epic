package epic.lexicon

import breeze.util.Index
import epic.constraints.TagConstraints

/**
 * A Lexicon tells you which tags are allowed at a particular point in a sentence.
 * See [[epic.lexicon.SimpleLexicon]].
 * @author dlwh
 */
trait Lexicon[L, W] extends TagConstraints.Factory[L, W] {
  def morePermissive: Lexicon[L, W]

  def labelIndex: Index[L]
  def anchor(w: IndexedSeq[W]):Anchoring

  // TODO, should i make TagConstraints be a case class instead of an interface?
  trait Anchoring extends TagConstraints[L] {
    def length: Int
    def allowedTags(pos: Int):Set[Int]
  }

}
