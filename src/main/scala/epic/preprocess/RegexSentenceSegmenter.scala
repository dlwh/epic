package epic.preprocess

import epic.slab._
import epic.trees.Span
import epic.slab.annotators.SentenceSegmenter

/**
 * A simple regex sentence segmenter.
 */
object RegexSentenceSegmenter extends SentenceSegmenter[Sentence] {
  def apply(content: String): Vector[Sentence] =
    // the [Sentence] is required because of https://issues.scala-lang.org/browse/SI-7647
    "[^\\s.!?]+([^.!?]+[.!?]|\\z)".r.findAllMatchIn(content).map(m => Sentence(Span(m.start, m.end))).toVector
}
