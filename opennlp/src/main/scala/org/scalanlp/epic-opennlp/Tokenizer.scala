package org.scalanlp.epic.opennlp

import epic.slab._
import opennlp.tools.tokenize._

import SpanToSpan._

class PToken(override val span: Span, val probability: Double) extends Token(span) with ProbabilityAnnotation
object PToken {
  def apply(s: Span, p: Double): PToken = new PToken(s, p)
  implicit val ptokenOffsetter = new Offsetter[PToken] {
    def apply(obj: PToken, by: Int) = PToken(obj.span.offset(by), obj.probability)
  }
}

class Tokenizer[S <: Sentence](val model: TokenizerModel) extends legacyannotators.Tokenizer[S, PToken, TokenizerME] {
  override val initialize = () => new TokenizerME(model)
  override def apply(tokenizer: TokenizerME, sentence: String) = {
    tokenizer.tokenizePos(sentence)
      .zip(tokenizer.getTokenProbabilities())
      .map({ case (span, prob) =>
        PToken(span, prob)
      })
  }
}

object Tokenizer {
  def apply(model: TokenizerModel): Tokenizer[PSentence] = new Tokenizer[PSentence](model)
}
