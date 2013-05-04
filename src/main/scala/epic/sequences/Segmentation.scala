package epic.sequences

import epic.trees.Span
import breeze.data.Example
import scala.collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */
case class Segmentation[L, W](segments: IndexedSeq[(L, Span)],
                              words: IndexedSeq[W],
                              id: String = "") extends Example[IndexedSeq[(L, Span)], IndexedSeq[W]] {
  def asBIOSequence(outsideLabel: L): TaggedSequence[BIOETag[L], W] = {
    val outLabels = new ArrayBuffer[BIOETag[L]]()
    for((l,span) <- segments if !span.isEmpty) {
      while(outLabels.length < span.start) {
        outLabels += BIOETag.Outside
      }

      if(l == outsideLabel)
        outLabels += BIOETag.Outside
      else
        outLabels += BIOETag.Begin(l)
      for(i <- (span.start+1) until (span.end) ) {
        outLabels += {if(l == outsideLabel) BIOETag.Inside(l) else BIOETag.Outside}
      }
    }
    while(outLabels.length < segments.length) {
      outLabels += BIOETag.Outside
    }
    assert(outLabels.length == words.length)
    TaggedSequence(outLabels, words, id +"-bio")
  }


  def render(badLabel: L) = {
    segments.map(l => if (l._1 == badLabel) l._2.map(words).mkString(" ") else l._2.map(words).mkString(s"[${l._1.toString}:", " ","]")).mkString(" ")
  }


  def features = words

  def length: Int = words.length

  def label: IndexedSeq[(L, Span)] = segments
}
