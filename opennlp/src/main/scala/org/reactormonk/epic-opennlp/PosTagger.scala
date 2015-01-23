package org.reactormonk.epic.opennlp

import epic.slab._
import epic.slab.Utils._
import shapeless._
import epic.trees.Span
import opennlp.tools.postag._
import opennlp.tools.util._
import SpanToSpan._

class PosTag(val tag: String, val probability: Double) extends ProbabilityAnnotation

object PosTag {
  def apply(tag: String, probability: Double): PosTag = new PosTag(tag, probability)
}

import aliases._

class PosTagger(
  val model: POSModel,
  val tagger: POSModel => POSTaggerME
) extends AnalysisFunctionN1[String, TaggerInput, Tagged[PosTag]] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(implicit sel: SelectMany.Aux[In, TaggerInput, TaggerInput], adder: Adder.Aux[In, Tagged[PosTag], Vector[Tagged[PosTag]], Out]): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val index = SpanIndex(data.select[Vector[PToken]])
    // Required because the API is not threadsafe.
    val tmodel = tagger(model)
    val annotatedSentences = for(sentence <- data.select[Vector[PSentence]]) yield {
      val tokens = index(sentence.span)
      val tags = tmodel.tag(tokens.map(x => slab.substring(x.span)).toArray)
      (tokens, tags, tmodel.probs()).zipped.map({case (token, tag, prob) => Tagged(token.span, PosTag(tag, prob))})
    }
    slab.add(annotatedSentences.flatten)(adder)
  }
}

object PosTagger {
  def tagger(beamSize: Int = POSTaggerME.DEFAULT_BEAM_SIZE): POSModel => POSTaggerME = { model =>
    new POSTaggerME(model, beamSize, 0)
  }
  def apply(model: POSModel): PosTagger = apply(model, PosTagger.tagger())
  def apply(
    model: POSModel,
    tagger: POSModel => POSTaggerME
  ): PosTagger = new PosTagger(model, tagger)
}
