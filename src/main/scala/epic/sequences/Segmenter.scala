package epic.sequences

import epic.slab._
import Utils._
import shapeless._
import ops.hlist._
import epic.trees.{Span, AnnotatedLabel}
import epic.slab.Implicits._

/**
 * A [[epic.sequences.Segmenter]] splits up a sentence into labeled segments. For instance, it might
 * find all the people, places and things (Named Entity Recognition) in a document.
 *
 * @tparam Tag the type of tag that is annotated
 *
 * @author dlwh
 **/
package object sequences {
  type input = Vector[Sentence] :: Vector[Token] :: HNil
}
import sequences._

class Segmenter[Tag](fun: (Vector[String] => Vector[Tagged[Tag]])) extends AnalysisFunctionN1[String, input, Tag] {
  def apply[In <: HList, Out <: HList](slab: Slab[String, In])(implicit sel: SelectMany.Aux[In, input, input], adder: Adder.Aux[In, Tagged[Tag], Vector[Tagged[Tag]], Out]): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val index = SpanIndex(slab.content, data.select[Vector[Token]])
    val annotatedSentences = for(sent <- data.select[Vector[Sentence]]) yield {
      val strings = index(sent.span).map(t => slab.substring(t.span))
      fun(strings).map(_.offset(sent.begin))
    }

    slab.add(annotatedSentences.flatten)(adder)
  }
}

object Segmenter {

  def nerSystem(crf: SemiCRF[String, String]) = fromCRF(crf, (a: String) => EntityMention(a))

  def fromCRF[L, Tag](crf: SemiCRF[L, String], lToTag: L=>Tag):Segmenter[Tag] = new SemiCRFSegmenter(crf, lToTag)

  case class SemiCRFSegmenter[L, Tag] (crf: SemiCRF[L, String], lToTag: L=>Tag) extends Segmenter[Tag]({
    v1 => crf.bestSequence(v1).segments.collect { case (l, span) if l != crf.outsideSymbol => Tagged(span, lToTag(l))}.toVector
  })
}
