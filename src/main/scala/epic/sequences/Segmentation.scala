package epic.sequences

import epic.trees.Span
import breeze.data.Example

/**
 *
 * @author dlwh
 */
case class Segmentation[L, W](segments: IndexedSeq[(L, Span)],
                              words: W, length: Int,
                              id: String = "") extends Example[IndexedSeq[(L, Span)], (W, Int)] {
  def features: (W, Int) = words -> length

  def label: IndexedSeq[(L, Span)] = segments
}
