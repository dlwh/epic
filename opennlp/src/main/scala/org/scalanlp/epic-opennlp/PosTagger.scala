package org.scalanlp.epic.opennlp

import epic.slab._
import epic.slab.typeclasses._
import shapeless._
import opennlp.tools.postag._
import SpanToSpan._

class PosTag(val tag: String, val probability: Double) extends ProbabilityAnnotation

object PosTag {
  def apply(tag: String, probability: Double): PosTag = new PosTag(tag, probability)
}

class PosTagger[S <: Sentence, T <: Token](
  val model: POSModel,
  val taggerinit: POSModel => POSTaggerME
) extends legacyannotators.Tagger[S, T, PosTag, POSTaggerME](
  () => taggerinit(model),
  {case(tmodel, tokens) =>
    val tags = tmodel.tag(tokens.toArray)
    (tags, tmodel.probs()).zipped.map({case (tag, prob) => PosTag(tag, prob)})
})

object PosTagger {
  def tagger(beamSize: Int = POSTaggerME.DEFAULT_BEAM_SIZE): POSModel => POSTaggerME = { model =>
    new POSTaggerME(model, beamSize, 0)
  }
  def apply(model: POSModel): PosTagger[PSentence, PToken] = apply(model, PosTagger.tagger())
  def apply(
    model: POSModel,
    tagger: POSModel => POSTaggerME
  ): PosTagger[PSentence, PToken] = new PosTagger[PSentence, PToken](model, tagger)
}
