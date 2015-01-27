package org.scalanlp.epic.opennlp

import epic.slab._
import shapeless._
import opennlp.tools.namefind._
import SpanToSpan._

class Tagger[S <: Sentence, T <: Token, A <: ProbabilityAnnotation](
  val model: TokenNameFinderModel,
  val tag: Double => A,
  val taggerinit: TokenNameFinderModel => NameFinderME
) extends legacyannotators.Segmenter[S, T, A, NameFinderME] {
  override val initialize = () => taggerinit(model)
  override def apply(model: NameFinderME, tokens: Vector[String]): Iterable[Tagged[A]] = {
    val spans = model.find(tokens.toArray)
    val tags = model.probs().map(tag)
    spans.zip(tags).map({case (span, tag) => Tagged[A](span, tag)})
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
  def apply[T <: ProbabilityAnnotation](model: TokenNameFinderModel, tag: Double => T): Tagger[PSentence, PToken, T] = apply(model, tag, Tagger.tagger())
  def apply[T <: ProbabilityAnnotation](
    model: TokenNameFinderModel,
    tag: Double => T,
    tagger: TokenNameFinderModel => NameFinderME
  ): Tagger[PSentence, PToken, T] = new Tagger[PSentence, PToken, T](model, tag, tagger)

  def date(model: TokenNameFinderModel) = apply(model, Date.apply _)
  def person(model: TokenNameFinderModel) = apply(model, Person.apply _)
  def organization(model: TokenNameFinderModel) = apply(model, Organization.apply _)
  def money(model: TokenNameFinderModel) = apply(model, Money.apply _)
  def location(model: TokenNameFinderModel) = apply(model, Location.apply _)
  def percentage(model: TokenNameFinderModel) = apply(model, Percentage.apply _)
  def time(model: TokenNameFinderModel) = apply(model, Time.apply _)
}
