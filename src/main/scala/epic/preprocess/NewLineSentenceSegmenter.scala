package epic.preprocess

import java.text.BreakIterator
import java.util.Locale
import epic.trees.Span
import epic.trees.SpanConvert._
import epic.slab._
import epic.slab.annotators.SentenceSegmenter

/**
 * TODO move to chalk
 *
 * @author dlwh
 **/
class NewLineSentenceSegmenter(locale: Locale = Locale.getDefault) extends SentenceSegmenter[Sentence] {
  def apply(content: String): Vector[Sentence] = {
    val breaker = BreakIterator.getLineInstance(locale)
    breaker.setText(content)
    new SegmentingIterator(breaker).map({ span =>
      Sentence(span)
    }).toVector
  }
}

class SegmentingIterator(inner: BreakIterator, private var start: Int = 0, private val last: Int = -1) extends Iterator[Span] {
  private var end = inner.following(start)

  def hasNext = (end != BreakIterator.DONE && (last == -1 || end <= last))

  def next = {
    val res = Span(start, end)
    start = end
    end = inner.next
    res
  }
}
