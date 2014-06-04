package epic.preprocess

import java.text.BreakIterator
import java.util.Locale
import epic.preprocess.SentenceSegmenter
import epic.trees.Span

/**
 * TODO move to chalk
 *
 * @author dlwh
 **/
class NewLineSentenceSegmenter(locale: Locale = Locale.getDefault) extends SentenceSegmenter {
  override def apply(v1: String): Iterable[String] = {
    new Iterable[String] {
      override def iterator: Iterator[String] = {
        val it = BreakIterator.getLineInstance(locale)
        it.setText(v1)
        new SegmentingIterator(it).map ( span => v1.substring(span.begin, span.end)  )
      }
    }
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
