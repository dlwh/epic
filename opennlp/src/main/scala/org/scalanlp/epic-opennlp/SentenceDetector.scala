package org.scalanlp.epic.opennlp

import epic.slab._
import opennlp.tools.util._
import epic.slab.Span
import opennlp.tools.sentdetect._
import SpanToSpan._

trait ProbabilityAnnotation {
  def probability: Double
}

class PSentence(override val span: Span, val probability: Double) extends Sentence(span) with ProbabilityAnnotation
object PSentence {
  def apply(s: Span, p: Double): PSentence = new PSentence(s, p)
}

class SentenceSegmenter(val model: SentenceModel) extends legacyannotators.SentenceSegmenter[PSentence, SentenceDetectorME] {
  override val initialize = () => new SentenceDetectorME(model)
  override def apply(detector: SentenceDetectorME, text: String): Iterable[PSentence] = {
    detector.sentPosDetect(text)
      .zip(detector.getSentenceProbabilities())
      .map({case (span, prob) => PSentence(span, prob)})
  }
}

object SentenceDetector {
  def apply(model: SentenceModel): SentenceSegmenter = new SentenceSegmenter(model)
}

object SpanToSpan {
  implicit def toEpic(span: opennlp.tools.util.Span): epic.slab.Span = Span(span.getStart, span.getEnd)
  implicit def toOpennlp(span: epic.slab.Span): opennlp.tools.util.Span = new opennlp.tools.util.Span(span.begin, span.end)
}
