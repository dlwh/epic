package epic.sequences

import epic.framework.Example
import epic.trees.Span

/**
 * A tagged sequence has a sequence of tags and a sequence of words that are in
 * one-to-one correspondence. think POS tags etc.
 * @author dlwh
 */
case class TaggedSequence[+L, +W](tags: IndexedSeq[L],
                                words: IndexedSeq[W],
                                id: String = "") extends Example[IndexedSeq[L], IndexedSeq[W]] {

  require(tags.length == words.length)

  def render = (tags zip words map { case (t, w) => w +"/" + t}).mkString(" ")
  def pairs = tags zip words
  def features = words
  def length: Int = words.length
  def label: IndexedSeq[L] = tags
  def asSegmentation = Segmentation(tags.zipWithIndex.map{case (l, i) => (l -> Span(i, i+1))}, words, id+"-seg")
}
