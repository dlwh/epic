package epic.preprocess

import java.text.BreakIterator
import java.util.Locale
import epic.preprocess.SentenceSegmenter

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
        new SegmentingIterator(it, v1)
      }
    }
  }
}

class SegmentingIterator(inner: BreakIterator, str: String) extends Iterator[String] {
  private var start = inner.first
  private var end = inner.next

  def hasNext = (end != BreakIterator.DONE)

  def next = {
    val res = str.substring(start, end)
    start = end
    end = inner.next
    res
  }
}
