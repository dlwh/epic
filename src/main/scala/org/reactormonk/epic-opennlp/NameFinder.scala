package org.reactormonk.epic.opennlp

import epic.slab._
import epic.slab.Utils._
import shapeless._
import epic.trees.Span
import opennlp.tools.namefind._
import opennlp.tools.util._
import SpanToSpan._

object aliases {
  type TaggerInput = Vector[PSentence] :: Vector[PToken] :: HNil
}
import aliases._

class Tagger[T <: ProbabilityAnnotation](
  val model: TokenNameFinderModel,
  val tag: Double => T,
  val tagger: TokenNameFinderModel => NameFinderME
) extends AnalysisFunctionN1[String, TaggerInput, Tagged[T]] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(
    implicit sel: SelectMany.Aux[In, TaggerInput, TaggerInput],
    adder: Adder.Aux[In, Tagged[T], Vector[Tagged[T]], Out]
  ): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val index = SpanIndex(data.select[Vector[PToken]])
    // Required because the API is not threadsafe.
    val tmodel = tagger(model)
    val annotatedSentences = for(sentence <- data.select[Vector[PSentence]]) yield {
      val tokens = index(sentence.span).map(s => slab.substring(s.span))
      val spans = tmodel.find(tokens.toArray).map(_.offset(sentence.begin))
      val tags = tmodel.probs().map(tag)
      spans.zip(tags).map({case (span, tag) => Tagged[T](span, tag)})
    }
    slab.add(annotatedSentences.flatten)(adder)
  }
}

case class Date(val probability: Double) extends ProbabilityAnnotation
case class Person(val probability: Double) extends ProbabilityAnnotation
case class Organization(val probability: Double) extends ProbabilityAnnotation
case class Money(val probability: Double) extends ProbabilityAnnotation
case class Location(val probability: Double) extends ProbabilityAnnotation
case class Percentage(val probability: Double) extends ProbabilityAnnotation
case class Time(val probability: Double) extends ProbabilityAnnotation

object Tagger {
  def tagger(beamSize: Int = NameFinderME.DEFAULT_BEAM_SIZE): TokenNameFinderModel => NameFinderME = { model =>
    new NameFinderME(model, beamSize)
  }
  def apply[T <: ProbabilityAnnotation](model: TokenNameFinderModel, tag: Double => T): Tagger[T] = apply(model, tag, Tagger.tagger())
  def apply[T <: ProbabilityAnnotation](
    model: TokenNameFinderModel,
    tag: Double => T,
    tagger: TokenNameFinderModel => NameFinderME
  ): Tagger[T] = new Tagger[T](model, tag, tagger)

  def date(model: TokenNameFinderModel) = apply(model, Date.apply _)
  def person(model: TokenNameFinderModel) = apply(model, Person.apply _)
  def organization(model: TokenNameFinderModel) = apply(model, Organization.apply _)
  def money(model: TokenNameFinderModel) = apply(model, Money.apply _)
  def location(model: TokenNameFinderModel) = apply(model, Location.apply _)
  def percentage(model: TokenNameFinderModel) = apply(model, Percentage.apply _)
  def time(model: TokenNameFinderModel) = apply(model, Time.apply _)
}
