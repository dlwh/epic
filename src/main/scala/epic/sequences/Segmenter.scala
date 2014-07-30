package epic.sequences

import epic.slab._
import epic.trees.Span

/**
 * A [[epic.sequences.Segmenter]] splits up a sentence into labeled segments. For instance, it might
 * find all the people, places and things (Named Entity Recognition) in a document.
 *
 * @tparam Tag the type of tag that is annotated
 *
 * @author dlwh
 **/
trait Segmenter[Tag] extends StringAnalysisFunction[Sentence with Token, Tag] with (IndexedSeq[String]=>IndexedSeq[(Tag, Span)]) {
  override def apply[In <: Sentence with Token](slab: StringSlab[In]): StringSlab[In with Tag] = {
    val annotatedSentences = for((span, sent) <- slab.iterator[Sentence]) yield {
      val tokens = slab.covered[Token](span).toIndexedSeq
      val tagSeq = apply(tokens.map(_._2.token))
      for( (lbl, espan) <- tagSeq) yield {
        Span(tokens(espan.begin)._1.begin, tokens(espan.end - 1)._1.end) -> lbl
      }
    }

    slab.++[Tag](annotatedSentences.flatten)
  }

}

object Segmenter {

  def nerSystem[L](crf: SemiCRF[L, String]) = fromCRF(crf, (a: L) => EntityMention(a.toString))

  def fromCRF[L, Tag](crf: SemiCRF[L, String], lToTag: L=>Tag):Segmenter[Tag] = new SemiCRFSegmenter(crf, lToTag)

  case class SemiCRFSegmenter[L, Tag] (crf: SemiCRF[L, String], lToTag: L=>Tag) extends Segmenter[Tag] {
    override def apply(v1: IndexedSeq[String]) = {
      crf.bestSequence(v1).segments.map { case (l, span) => lToTag(l) -> span}
    }
  }
}
