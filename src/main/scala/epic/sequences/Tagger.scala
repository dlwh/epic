package epic.sequences

import epic.slab._
import epic.slab.Implicits
import epic.slab.Utils._
import shapeless._
import ops.hlist._
import epic.slab.Indexes._
import epic.trees.AnnotatedLabel

/**
 * A Tagger assigns a sequence of Tags to a
 *
 * @tparam Tag the type of tag that is annotated
 *
 * @author dlwh
 **/
trait Tagger[Tag] extends AnalysisFunction11[String, Sentence, Token] {
  def apply(v1: Vector[String]): Vector[Tag]

  def apply[In <: HList, Out <: HList](slab: StringSlab[In])(implicit sel: Selector[In, Vector[Token]], adder: Adder.Aux[In, Tagged[Tag], Vector[Tagged[Tag]], Out]): Slab[String, Out] = {
    val annotatedSentences = for(token <- slab.get[Token]) yield {
      val tokens = slab.covered[Token](token.span)
      val strings = tokens.map(t => slab.at(t.span))
      val tagSeq = apply(strings)
      tokens.zip(tagSeq).map({case (token, tag) => Tagged[Tag](token.span, tag)})
    }

    slab.add(annotatedSentences.flatten)(adder)
  }

}

object Tagger {

  def posTagger(crf: CRF[AnnotatedLabel, String]) = fromCRF(crf, (a: AnnotatedLabel) => a.label)

  def fromCRF[L, Tag](crf: CRF[L, String], lToTag: L=>Tag):Tagger[Tag] = new CRFTagger(crf, lToTag)

  case class CRFTagger[L, Tag] (crf: CRF[L, String], lToTag: L=>Tag) extends Tagger[Tag] {
    override def apply(v1: IndexedSeq[String]): IndexedSeq[Tag] = {
      crf.bestSequence(v1).tags.map(lToTag)
    }
  }
}
