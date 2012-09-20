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
    segments.filter(_._1 != badLabel).map(l => l._2.map(words).mkString(l._1.toString+": [", " ","]")).mkString("\n")
  }


  def features = words

  def length: Int = words.length

  def label: IndexedSeq[(L, Span)] = segments
}
