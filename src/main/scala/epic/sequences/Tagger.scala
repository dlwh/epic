package epic.sequences

import epic.slab._
import epic.trees.AnnotatedLabel

import scala.reflect.ClassTag

/**
 * A Tagger assigns a sequence of Tags to a
 *
 * @tparam Tag the type of tag that is annotated
 *
 * @author dlwh
 **/
trait Tagger[Tag] extends StringAnalysisFunction[Sentence with Token, Tag] with (IndexedSeq[String]=>IndexedSeq[Tag]) {
  implicit protected def tagTag: ClassTag[Tag]
  override def apply[In <: Sentence with Token](slab: StringSlab[In]): StringSlab[In with Tag] = {
    val annotatedSentences = for((span, sent) <- slab.iterator[Sentence]) yield {
      val tokens = slab.covered[Token](span)
      val tagSeq = apply(tokens.map(_._2.token))
      tokens.map(_._1) zip tagSeq
    }
    slab.addLayer[Tag](annotatedSentences.flatten)
  }
}

object Tagger {

  def posTagger(crf: CRF[AnnotatedLabel, String]) = fromCRF(crf, (a: AnnotatedLabel) => PartOfSpeech(a.label))

  def fromCRF[L, Tag:ClassTag](crf: CRF[L, String], lToTag: L=>Tag):Tagger[Tag] = new CRFTagger(crf, lToTag)

  case class CRFTagger[L, Tag:ClassTag] (crf: CRF[L, String], lToTag: L=>Tag) extends Tagger[Tag] {
    protected def tagTag: ClassTag[Tag] = implicitly[ClassTag[Tag]]
    override def apply(v1: IndexedSeq[String]): IndexedSeq[Tag] = {
      crf.bestSequence(v1).tags.map(lToTag)
    }
  }
}
