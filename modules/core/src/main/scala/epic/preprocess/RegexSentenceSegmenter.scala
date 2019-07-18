package epic.preprocess

import epic.slab._
import epic.trees.Span

/**
 * A simple regex sentence segmenter.
 */
object RegexSentenceSegmenter extends SentenceSegmenter {

  def apply[In](slab: StringSlab[In]) =
    // the [Sentence] is required because of https://issues.scala-lang.org/browse/SI-7647
    slab.addLayer[Sentence]("[^\\s.!?]+([^.!?]+[.!?]|\\z)".r.findAllMatchIn(slab.content).map(m => Span(m.start, m.end) -> Sentence()))
}
