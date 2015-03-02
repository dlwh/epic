package epic.preprocess

import java.text.BreakIterator
import java.util.Locale
import java.util.regex.Pattern
import epic.trees.Span
import epic.slab._
import epic.slab.annotators.SentenceSegmenter

import scala.collection.mutable.ArrayBuffer

class NewLineSentenceSegmenter(locale: Locale = Locale.getDefault) extends SentenceSegmenter[Sentence] {

  private val regex = Pattern.compile("\n+")

  def apply(content: String): Vector[Sentence] = {
    val m = regex.matcher(content)

    val spans = new ArrayBuffer[Sentence]()

    var start = 0
    while(m.find()) {
      val end = m.end()
      if(end - start > 1)
        spans += Sentence(Span(start, end))
      start = end
    }
    spans += Sentence(Span(start, content.length))
    spans.toVector
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
