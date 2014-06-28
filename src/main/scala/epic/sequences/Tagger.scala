package epic.sequences

import epic.slab._

/**
 * TODO
 *
 * @author dlwh
 **/
trait Tagger[Tag <: AnnotatedSpan] extends StringAnalysisFunction[Sentence with Token, Tag] with (IndexedSeq[String]=>IndexedSeq[Tag]) {
  override def apply[In <: Sentence with Token](slab: Slab[String, AnnotatedSpan, In]): Slab[String, AnnotatedSpan, In with Tag] = {
    val annotatedSentences = for(s <- slab.iterator[Sentence]) yield {
      val tokens = s.in(slab).covered[Token].map(_.token).toIndexedSeq
      val tagSeq = apply(tokens)
      tagSeq
    }

    slab.++[Tag](annotatedSentences.flatten)
  }

}

object Tagger {
  def fromCRF[L, Tag <: AnnotatedSpan](crf: CRF[L, String], lToTag: L=>Tag):Tagger[Tag] = new CRFTagger(crf, lToTag)

  case class CRFTagger[L, Tag <: AnnotatedSpan](crf: CRF[L, String], lToTag: L=>Tag) extends Tagger[Tag] {
    override def apply(v1: IndexedSeq[String]): IndexedSeq[Tag] = {
      crf.bestSequence(w).tags.map(lToTag)
    }
  }
}
