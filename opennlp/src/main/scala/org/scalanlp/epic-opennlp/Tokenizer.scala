package org.scalanlp.epic.opennlp

import epic.slab._
import epic.trees.Span
import opennlp.tools.util._
import opennlp.tools.tokenize._

import SpanToSpan._

class PToken(override val span: Span, val probability: Double) extends Token(span) with ProbabilityAnnotation
object PToken {
  def apply(s: Span, p: Double): PToken = new PToken(s, p)
}

class Tokenizer(val model: TokenizerModel) extends AnalysisFunction11[String, PSentence, PToken] {
  def apply(text: String, sentences: Vector[PSentence]) = {
    val tokenizer = new TokenizerME(model)
      sentences.map({ s =>
        tokenizer.tokenizePos(text.substring(s.begin, s.end))
          .zip(tokenizer.getTokenProbabilities())
          .map({ case (span, prob) =>
            PToken(span.offset(s.begin), prob)
        })
      }).flatten
  }
}

object Tokenizer {
  def apply(model: TokenizerModel): Tokenizer = new Tokenizer(model)
}
