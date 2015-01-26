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

object caliases {
  type ChunkerInput = List[Tagged[PosTag]] :: List[PSentence] :: List[PToken] :: HNil
}
import caliases._

class Chunker(
  val model: ChunkerModel,
  val chunker: ChunkerModel => ChunkerME
) extends AnalysisFunctionN1[String, ChunkerInput, Tagged[PosTag]] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(
    implicit sel: SubSelectMany.Aux[In, ChunkerInput, ChunkerInput],
    adder: Adder.Aux[In, List[Tagged[PosTag]], Out]
  ): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val tokenIndex = SpanIndex(data.select[List[PToken]])
    val posIndex = SpanIndex(data.select[List[Tagged[PosTag]]])
    // Required because the API is not threadsafe.
    val cmodel = chunker(model)
    val annotatedSentences = for(sentence <- data.select[List[PSentence]]) yield {
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

  def apply(model: ChunkerModel): Chunker = apply(model, Chunker.chunker())
  def apply(
    model: ChunkerModel,
    tagger: ChunkerModel => ChunkerME
  ): Chunker = new Chunker(model, tagger)
}