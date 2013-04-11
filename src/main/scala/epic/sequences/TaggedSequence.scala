package epic.sequences

import epic.trees.Span
import breeze.data.Example

/**
 *
 * @author dlwh
 */
case class TaggedSequence[L, W](tags: IndexedSeq[L],
                                words: IndexedSeq[W],
                                id: String = "") extends Example[IndexedSeq[L], IndexedSeq[W]] {

  require(tags.length == words.length)

  def render(badLabel: L) = {
    (tags zip words map { case (t, w) => w +"/" + t}).mkString(" ")
  }

  def features = words

  def length: Int = words.length

  def label: IndexedSeq[L] = tags
}
