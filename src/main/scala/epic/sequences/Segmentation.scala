package epic.sequences

import epic.trees.Span
import breeze.data.Example

/**
 *
 * @author dlwh
 */
case class Segmentation[L, W](segments: IndexedSeq[(L, Span)],
                              words: IndexedSeq[W],
                              id: String = "") extends Example[IndexedSeq[(L, Span)], IndexedSeq[W]] {


  def render(badLabel: L) = {
    segments.map(l => if (l._1 == badLabel) l._2.map(words).mkString(" ") else l._2.map(words).mkString(s"[${l._1.toString}:", " ","]")).mkString(" ")
  }


  def features = words

  def length: Int = words.length

  def label: IndexedSeq[(L, Span)] = segments
}
