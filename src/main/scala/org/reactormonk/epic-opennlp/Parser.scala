package org.reactormonk.epic.opennlp

import epic.slab._
import epic.slab.Utils._
import shapeless._
import epic.trees.Span
import opennlp.tools.parser._
import opennlp.tools.util._
import SpanToSpan._
import scala.collection.JavaConversions._

class ParseTag(val span: epic.trees.Span, val parseType: String, val probability: Double) extends ProbabilityAnnotation with SpanAnnotation

object ParseTag {
  def apply(span: Span, tag: String, probability: Double): ParseTag = new ParseTag(span, tag, probability)
}

import aliases._

class Parser(
  val model: ParserModel,
  val parser: ParserModel => opennlp.tools.parser.Parser
) extends AnalysisFunctionN1[String, TaggerInput, ParseTag] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(
    implicit sel: SelectMany.Aux[In, TaggerInput, TaggerInput],
    adder: Adder.Aux[In, ParseTag, Vector[ParseTag], Out]
  ): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val index = SpanIndex(data.select[Vector[PToken]])
    // Required because the API is not threadsafe.
    val pmodel = parser(model)
    val annotatedSentences: Vector[List[ParseTag]] = data.select[Vector[PSentence]].flatMap({ sentence =>
      val s = slab.substring(sentence.span)
      val tokens = index(sentence.span)
      val unparsed = new Parse(s, Span(0, sentence.end - sentence.begin), "INC", 1, null)
      tokens.map(t => unparsed.insert(new Parse(s, t.span.offset(-sentence.begin), "TK", 0.0, 0)))
      val parsed = if(unparsed.getChildCount > 0) { Some(pmodel.parse(unparsed)) } else { None }
      parsed.map(p => transform(List(p), sentence.begin))
    })
    slab.add(annotatedSentences.flatten)(adder)
  }

  def transform(parses: List[Parse], offset: Int): List[ParseTag] = {
    parses.flatMap({child =>
      List(ParseTag(child.getSpan().offset(offset), child.getType, child.getProb)) ++ transform(child.getChildren.toList, offset)
    })
  }
}

object Parser {
  def parser(): ParserModel => opennlp.tools.parser.Parser = { model => ParserFactory.create(model)}
  def apply(model: ParserModel): Parser = apply(model, Parser.parser())
  def apply(
    model: ParserModel,
    tagger: ParserModel => opennlp.tools.parser.Parser
  ): Parser = new Parser(model, tagger)
}
