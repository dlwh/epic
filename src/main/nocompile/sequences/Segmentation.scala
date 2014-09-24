package epic.sequences

import epic.trees.{Tree, Span}
import scala.collection.mutable.ArrayBuffer
import nak.data.Example

/**
 *
 * @author dlwh
 */
case class Segmentation[+L, +W](segments: IndexedSeq[(L, Span)],
                              words: IndexedSeq[W],
                              id: String = "") extends Example[IndexedSeq[(L, Span)], IndexedSeq[W]] {



  def render[LL>:L](badLabel: LL) = {
    segments.map(l => if (l._1 == badLabel) l._2.map(words).mkString(" ") else l._2.map(words).mkString(s"[${l._1.toString}:", " ","]")).mkString(" ")
  }


  def features = words

  def length: Int = words.length

  def label: IndexedSeq[(L, Span)] = segments

  def asBIOSequence[LL>:L](outsideLabel: LL): TaggedSequence[BIOETag[L], W] = {
    val outLabels = new ArrayBuffer[BIOETag[L]]()
    for((l,span) <- segments if !span.isEmpty) {
      while(outLabels.length < span.begin) {
        outLabels += BIOETag.Outside
      }

      if(l == outsideLabel)
        outLabels += BIOETag.Outside
      else
        outLabels += BIOETag.Begin(l)
      for(i <- (span.begin+1) until (span.end) ) {
        outLabels += {if(l != outsideLabel) BIOETag.Inside(l) else BIOETag.Outside}
      }
    }
    while(outLabels.length < words.length) {
      outLabels += BIOETag.Outside
    }
    assert(outLabels.length == words.length)
    TaggedSequence(outLabels, words, id +"-bio")
  }

  def asFlatTaggedSequence[LL>:L](outsideLabel: LL): TaggedSequence[LL, W] = {
    val outLabels = new ArrayBuffer[LL]()
    for((l,span) <- segments if !span.isEmpty) {
      while(outLabels.length < span.begin) {
        outLabels += outsideLabel
      }

      for(i <- (span.begin) until (span.end) ) {
        outLabels += l
      }
    }
    while(outLabels.length < words.length) {
      outLabels += outsideLabel
    }
    assert(outLabels.length == words.length)
    TaggedSequence(outLabels, words, id +"-flattened")
  }

  def toTree[LL>:L](outsideLabel: LL):Tree[LL] = {
    val outLabels = new ArrayBuffer[(LL, Span)]()
    for((l,span) <- segments if !span.isEmpty) {
      val lastEnd = outLabels.lastOption.map(_._2.end).getOrElse(0)
      if(lastEnd < span.begin) {
        outLabels += (outsideLabel -> Span(lastEnd, span.begin))
      }

      outLabels += (l -> span)
    }

    val lastEnd = outLabels.lastOption.map(_._2.end).getOrElse(0)
    if(lastEnd < words.length) {
      outLabels += (outsideLabel -> Span(lastEnd, words.length))
    }

    val t = Tree(outsideLabel, outLabels.map { case (l, span) => Tree(l, IndexedSeq.empty, span)}, Span(0, words.length))
    assert(t.isValid, outLabels)
    t
  }
}

object Segmentation {
  def fromBIOSequence[L, W](seq: TaggedSequence[BIOETag[L], W], outsideLabel: L):Segmentation[L, W] = {
    import BIOETag._
    val spans = ArrayBuffer[(L, Span)]()
    var currentStart = 0
    var currentLabel = outsideLabel
    for(i <- 0 until seq.length) {
      seq.label(i) match {
        case Begin(l) =>
          if(currentStart < i)
           spans += (currentLabel -> Span(currentStart, i))
          currentStart = i
          currentLabel = l
        case Inside(l) =>
          if(currentLabel != l) {
            if(currentStart < i)
              spans += (currentLabel -> Span(currentStart, i))
            currentStart = i
            currentLabel = l
          }
        case End(l) =>
          if(currentLabel != l) {
            if(currentStart < i)
              spans += (currentLabel -> Span(currentStart, i))
            currentStart = i
            currentLabel = l
          }
          spans += (currentLabel -> Span(currentStart, i+1))
          currentStart = i + 1
        case Outside =>
          if(currentLabel != outsideLabel) {
            if(currentStart < i)
              spans += (currentLabel -> Span(currentStart, i))
            currentStart = i
            currentLabel = outsideLabel
          }
          spans += (currentLabel -> Span(currentStart, i+1))
          currentStart = i + 1

      }
    }
    if(currentStart < seq.length)
      spans += (currentLabel -> Span(currentStart, seq.length))
    Segmentation(spans, seq.words, seq.id.replaceAll("-bio","-seg"))
  }
}
