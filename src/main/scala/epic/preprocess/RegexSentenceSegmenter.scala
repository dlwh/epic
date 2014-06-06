package epic.preprocess

import epic.slab._

/**
 * A simple regex sentence segmenter.
 */
object RegexSentenceSegmenter extends SentenceSegmenter {

  def apply[In <: AnnotatedSpan](slab: StringSlab[In]) =
    // the [Sentence] is required because of https://issues.scala-lang.org/browse/SI-7647
    slab.++[Sentence]("[^\\s.!?]+([^.!?]+[.!?]|\\z)".r.findAllMatchIn(slab.content).map(m => Sentence(m.start, m.end)))
}
