package epic.preprocess

import java.text.BreakIterator
import java.util.Locale
import java.util.regex.Pattern
import epic.trees.Span
import epic.slab._
import epic.slab.Sentence

import scala.collection.mutable.ArrayBuffer

class NewLineSentenceSegmenter(locale: Locale = Locale.getDefault) extends SentenceSegmenter {

  private val regex = Pattern.compile("\n+")

  override def apply[In](slab: StringSlab[In]): StringSlab[In with Sentence] = {
    val m = regex.matcher(slab.content)

    val spans = new ArrayBuffer[(Span, Sentence)]()

    var start = 0
    while (m.find()) {
      val end = m.end()
      if (end - start > 1)
        spans += (Span(start, end) -> Sentence())
      start = end
    }
    spans += Span(start, slab.content.length) -> Sentence()

    slab.addLayer[Sentence](spans)
  }
}

class SegmentingIterator(inner: BreakIterator, private var start: Int = 0, private val last: Int = -1) extends Iterator[Span] {
  private var end = inner.following(start)

  def hasNext = end != BreakIterator.DONE && (last == -1 || end <= last)

  def next = {
    val res = Span(start, end)
    start = end
    end = inner.next
    res
  }
}
