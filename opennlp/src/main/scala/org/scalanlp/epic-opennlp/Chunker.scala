package org.scalanlp.epic.opennlp

import epic.slab._
import epic.slab.typeclasses._
import shapeless._
import opennlp.tools.chunker._
import SpanToSpan._

class Chunk(val tag: String, val probability: Double) extends ProbabilityAnnotation

object Chunk {
  def apply(tag: String, probabilitiy: Double): Chunk = new Chunk(tag, probabilitiy)
}

class Chunker[S <: Sentence, T <: Token, P <: Tagged[PosTag]] (
  val model: ChunkerModel,
  val chunker: ChunkerModel => ChunkerME
) extends AnalysisFunctionN1[String, List[S] :: List[T] :: List[P] :: HNil, Tagged[PosTag]] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(
    implicit sel: SelectMany.Aux[In, List[S] :: List[T] :: List[P] :: HNil, List[S] :: List[T] :: List[P] :: HNil],
    adder: Adder.Aux[In, List[Tagged[PosTag]], Out]
  ): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val tokenIndex = SpanIndex(data.select[List[T]])
    val posIndex = SpanIndex(data.select[List[P]])
    // Required because the API is not threadsafe.
    val cmodel = chunker(model)
    val annotatedSentences = for(sentence <- data.select[List[S]]) yield {
      val tokens = tokenIndex(sentence.span)
      val pos = posIndex(sentence.span)
      val tags = cmodel.chunk(tokens.map(s => slab.substring(s.span)).toArray, pos.map(_.tag.tag).toArray)
      (tokens, tags, cmodel.probs()).zipped.map({case (token, tag, prob) => Tagged(token.span.offset(sentence.begin), PosTag(tag, prob))})
    }
    slab.add(annotatedSentences.flatten)(adder)
  }
}

object Chunker {
  def chunker(): ChunkerModel => ChunkerME = { model =>
    new ChunkerME(model)
  }

  def apply(model: ChunkerModel): Chunker[PSentence, PToken, Tagged[PosTag]] = apply(model, Chunker.chunker())
  def apply(
    model: ChunkerModel,
    tagger: ChunkerModel => ChunkerME
  ): Chunker[PSentence, PToken, Tagged[PosTag]] = new Chunker[PSentence, PToken, Tagged[PosTag]](model, tagger)
}
