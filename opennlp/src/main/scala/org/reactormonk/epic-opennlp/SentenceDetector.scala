package org.reactormonk.epic.opennlp

import epic.slab._
import opennlp.tools.util._
import epic.slab.Span
import opennlp.tools.sentdetect._
import SpanToSpan._
import epic.slab.legacyannotators._

trait ProbabilityAnnotation {
  def probability: Double
}

class PSentence(override val span: Span, val probability: Double) extends Sentence(span) with ProbabilityAnnotation
object PSentence {
  def apply(s: Span, p: Double): PSentence = new PSentence(s, p)
}

class SentenceDetector(val model: SentenceModel) extends SentenceSegmenter {
  def apply(text: String): Vector[PSentence] = {
    // The detector is not threadsafe
    val detector = new SentenceDetectorME(model) 
    detector.sentPosDetect(text)
      .zip(detector.getSentenceProbabilities())
      .map({case (span, prob) => PSentence(span, prob)})
      .toVector
  }
}

object SentenceDetector {
  def apply(model: SentenceModel): SentenceDetector = new SentenceDetector(model)
}

object SpanToSpan {
  implicit def toEpic(span: opennlp.tools.util.Span): epic.trees.Span = Span(span.getStart, span.getEnd)
  implicit def toOpennlp(span: epic.trees.Span): opennlp.tools.util.Span = new opennlp.tools.util.Span(span.begin, span.end)
}
