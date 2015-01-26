package epic.sequences

import epic.slab._
import epic.slab.annotators.{Segmenter => SlabSegmenter}
import epic.trees.{Span, AnnotatedLabel}
import epic.trees.SpanConvert._

object Segmenter {
  def nerSystem(crf: SemiCRF[String, String]) = fromCRF(crf, (a: String) => EntityMention(a))

  def fromCRF[L, Tag](crf: SemiCRF[L, String], lToTag: L=>Tag): SlabSegmenter[Sentence, Token, Tag] = new SemiCRFSegmenter(crf, lToTag)

  case class SemiCRFSegmenter[S <: Sentence, T <: Token, L, Tag] (crf: SemiCRF[L, String], lToTag: L=>Tag) extends SlabSegmenter[S, T, Tag] {
    override def apply(v1: Vector[String]): Vector[Tagged[Tag]] = crf.bestSequence(v1).segments.collect { case (l, span) => Tagged(span, lToTag(l))}.toVector
  }

  def apply[Tag](seg: Vector[String] => Vector[Tagged[Tag]]) = new SlabSegmenter[Sentence, Token, Tag] {
    override def apply(sentence: Vector[String]): Vector[Tagged[Tag]] = seg(sentence)
 }
}
