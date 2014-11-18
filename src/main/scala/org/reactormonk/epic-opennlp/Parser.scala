package org.reactormonk.epic.opennlp

import epic.slab._
import epic.slab.Utils._
import shapeless._
import epic.trees.Span
import opennlp.tools.parser._
import opennlp.tools.util._
import SpanToSpan._
import scala.collection.JavaConversions._

class ParseTag(val parseType: String, val probability: Double, val parent: Option[Tagged[ParseTag]]) extends ProbabilityAnnotation

object ParseTag {
  def apply(tag: String, probability: Double, parent: Option[Tagged[ParseTag]]): ParseTag = new ParseTag(tag, probability, parent)
}

import aliases._

class Parser(
  val model: ParserModel,
  val parser: ParserModel => opennlp.tools.parser.Parser
) extends AnalysisFunctionN1[String, TaggerInput, Tagged[ParseTag]] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(
    implicit sel: SelectMany.Aux[In, TaggerInput, TaggerInput],
    adder: Adder.Aux[In, Tagged[ParseTag], Vector[Tagged[ParseTag]], Out]
  ): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val index = SpanIndex(data.select[Vector[PToken]])
    // Required because the API is not threadsafe.
    val pmodel = parser(model)
    val annotatedSentences: Vector[List[Tagged[ParseTag]]] = data.select[Vector[PSentence]].flatMap({ sentence =>
      val s = slab.substring(sentence.span)
      val tokens = index(sentence.span)
      val unparsed = new Parse(s, Span(0, sentence.end - sentence.begin), "INC", 1, null)
      tokens.map(t => unparsed.insert(new Parse(s, t.span.offset(-sentence.begin), "TK", 0.0, 0)))
      val parsed = if(unparsed.getChildCount > 0) { Some(pmodel.parse(unparsed)) } else { None }
      parsed.map(p => transform(p).map(_.offset(sentence.begin)))
    })
    slab.add(annotatedSentences.flatten)(adder)
  }

  def transform(parse: Parse): List[Tagged[ParseTag]] = {
    val parent = Tagged(parse.getSpan(), ParseTag(parse.getType, parse.getProb, None))
    transformRecurse(parse, parent)
  }

  def transformRecurse(parse: Parse, parent: Tagged[ParseTag]): List[Tagged[ParseTag]] = {
    parse.getChildren.map({child =>
      val childTag = Tagged(child.getSpan(), ParseTag(child.getType, child.getProb, Some(parent)))
      List(parent) ++ transformRecurse(child, childTag)
    }).toList.flatten
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
