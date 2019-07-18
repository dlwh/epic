package epic.sequences

import epic.framework.Example
import epic.trees.{Tree, Span}
import scala.collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */
case class Segmentation[+L, +W](segments: IndexedSeq[(L, Span)],
                                words: IndexedSeq[W],
                                id: String = "") extends Example[IndexedSeq[(L, Span)], IndexedSeq[W]] {

  def render: String = {
    segmentsWithOutside.map {
      case (None, span) => words.slice(span.begin, span.end).mkString(" ")
      case (Some(l), span) => words.slice(span.begin, span.end).mkString(s"[$l: ", " ", "]")
    }.mkString(" ")
  }

  def features = words

  def length: Int = words.length

  def label: IndexedSeq[(L, Span)] = segments

  def mapWords[W2](f: (W) => W2) = copy(words = words.map(f))

  def filterLabels(f: (L) => Boolean) = Segmentation(segments.filter { case (l, span) => f(l)}, words, id + "-filtered")

  def filterWords(f: (W) => Boolean):Segmentation[L, W] = {
    val newWords = new ArrayBuffer[W]()
    val newSegments = new ArrayBuffer[(L, Span)]()

    var newOffset = 0
    var currentSegment = 0
    var newSpanBegin = 0

    for (i <- 0 until length) {

      if (currentSegment < segments.length && segments(currentSegment)._2.end == i) {
        if (newSpanBegin != newOffset)
          newSegments += (segments(currentSegment)._1 -> Span(newSpanBegin, newOffset))

        currentSegment += 1
      }

      if (currentSegment < segments.length && segments(currentSegment)._2.begin == i) {
        newSpanBegin = newOffset
      }

      if (f(words(i))) {
        newWords += words(i)
        newOffset += 1
      }

    }

    new Segmentation(newSegments, newWords, s"$id-filtered")
  }

  def segmentsWithOutside: Iterator[(Option[L], Span)] = {
    val segs = for {
      qq@IndexedSeq((pL, pSpan), (l, span)) <- (segments.headOption.map(pair => pair._1 -> Span(0, 0)).toIndexedSeq ++ segments).sliding(2)
      padding = Iterator.range(pSpan.end, span.begin).map(i => None -> Span(i, i + 1))
      pair <- padding ++ Iterator((Some(l), span))
    } yield {
      pair
    }

    val lastSpanEnd = segments.lastOption match {
      case Some((_, Span(_, end))) => end
      case _ => 0
    }

    segs ++ (lastSpanEnd until length).map(i => None -> Span(i, i + 1))
  }

  def asBIOSequence[LL>:L](outsideLabel: LL): TaggedSequence[BIOETag[L], W] = {
    val outLabels = new ArrayBuffer[BIOETag[L]]()
    for((l,span) <- segments if !span.isEmpty) {
      while (outLabels.length < span.begin) {
        outLabels += BIOETag.Outside
      }
      if (l == outsideLabel)
        outLabels += BIOETag.Outside
      else
        outLabels += BIOETag.Begin(l)
      for(i <- (span.begin+1) until (span.end) ) {
        outLabels += {if (l != outsideLabel) BIOETag.Inside(l) else BIOETag.Outside}
      }
    }
    while (outLabels.length < words.length) {
      outLabels += BIOETag.Outside
    }
    assert(outLabels.length == words.length)
    TaggedSequence(outLabels, words, id +"-bio")
  }

  def asFlatTaggedSequence[LL>:L]: TaggedSequence[Option[LL], W] = {
    val outLabels = new ArrayBuffer[Option[LL]]()
    for((l,span) <- segments if !span.isEmpty) {
      while (outLabels.length < span.begin) {
        outLabels += None
      }

      for(i <- (span.begin) until (span.end) ) {
        outLabels += Some(l)
      }
    }
    while (outLabels.length < words.length) {
      outLabels += None
    }
    assert(outLabels.length == words.length)
    TaggedSequence(outLabels, words, id +"-flattened")
  }

  def toTree[LL>:L](outsideLabel: LL):Tree[LL] = {
    val outLabels = new ArrayBuffer[(LL, Span)]()
    for((l,span) <- segments if !span.isEmpty) {
      val lastEnd = outLabels.lastOption.map(_._2.end).getOrElse(0)
      if (lastEnd < span.begin) {
        outLabels += (outsideLabel -> Span(lastEnd, span.begin))
      }
      outLabels += (l -> span)
    }

    val lastEnd = outLabels.lastOption.map(_._2.end).getOrElse(0)
    if (lastEnd < words.length) {
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
          if (currentStart < i)
            spans += (currentLabel -> Span(currentStart, i))
          currentStart = i
          currentLabel = l
        case Inside(l) =>
          if (currentLabel != l) {
            if (currentStart < i)
              spans += (currentLabel -> Span(currentStart, i))
            currentStart = i
            currentLabel = l
          }
        case End(l) =>
          if (currentLabel != l) {
            if (currentStart < i)
              spans += (currentLabel -> Span(currentStart, i))
            currentStart = i
            currentLabel = l
          }
          spans += (currentLabel -> Span(currentStart, i+1))
          currentStart = i + 1
        case Outside =>
          if (currentLabel != outsideLabel) {
            if (currentStart < i)
              spans += (currentLabel -> Span(currentStart, i))
            currentStart = i
            currentLabel = outsideLabel
          }
          spans += (currentLabel -> Span(currentStart, i+1))
          currentStart = i + 1
      }
    }
    if (currentStart < seq.length)
      spans += (currentLabel -> Span(currentStart, seq.length))
    Segmentation(spans, seq.words, seq.id.replaceAll("-bio","-seg"))
  }
}
