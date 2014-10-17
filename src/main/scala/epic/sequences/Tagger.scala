package epic.sequences

import epic.slab._
import epic.trees.AnnotatedLabel

/**
 * A Tagger assigns a sequence of Tags to a
 *
 * @tparam Tag the type of tag that is annotated
 *
 * @author dlwh
 **/
trait Tagger[Tag] extends StringAnalysisFunction with (Vector[String]=>Vector[Tag]) {
  def apply[In <: HList, Out <: HList](slab: StringSlab[In])(implicit sel: Selector[In, Vector[Sentence]], adder: Adder.Aux[In, Token, Vector[Tag], Out]): Slab[C, Out] = {
    val annotatedSentences = for((span, sent) <- slab.iterator[Sentence]) yield {
      val tokens = slab.covered[Token](span).toIndexedSeq
      val tagSeq = apply(tokens.map(_._2.token))
      tokens.map(_._1) zip tagSeq
    }

    slab.++[Tag](annotatedSentences.flatten)
  }

}

object Tagger {

  def posTagger(crf: CRF[AnnotatedLabel, String]) = fromCRF(crf, (a: AnnotatedLabel) => PartOfSpeech(a.label))

  def fromCRF[L, Tag](crf: CRF[L, String], lToTag: L=>Tag):Tagger[Tag] = new CRFTagger(crf, lToTag)

  case class CRFTagger[L, Tag] (crf: CRF[L, String], lToTag: L=>Tag) extends Tagger[Tag] {
    override def apply(v1: IndexedSeq[String]): IndexedSeq[Tag] = {
      crf.bestSequence(v1).tags.map(lToTag)
    }
  }
}
