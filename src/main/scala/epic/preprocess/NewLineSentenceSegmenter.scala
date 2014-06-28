package epic.preprocess

import java.text.BreakIterator
import java.util.Locale
import epic.trees.Span
import epic.slab._
import epic.slab.Sentence

/**
 * TODO move to chalk
 *
 * @author dlwh
 **/
class NewLineSentenceSegmenter(locale: Locale = Locale.getDefault) extends SentenceSegmenter {

  override def apply[In](slab: StringSlab[In]): StringSlab[In with Sentence] = {
    val breaker = BreakIterator.getLineInstance(locale)
    breaker.setText(slab.content)
    slab.++[Sentence](
      new SegmentingIterator(breaker).map { span =>
        span -> Sentence()
      }
    )
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
