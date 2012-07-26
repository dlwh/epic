package epic.coref

import epic.ontonotes.{Document, Sentence, Mention}
import epic.trees.Span
import breeze.data.Example

/**
 *
 * @author dlwh
 */

case class CorefInstance(id: String,
                         clusters: Set[Set[MentionCandidate]],
                         mentions: IndexedSeq[MentionCandidate],
                         words: IndexedSeq[IndexedSeq[String]]) extends Example[Set[Set[MentionCandidate]], (IndexedSeq[MentionCandidate],IndexedSeq[IndexedSeq[String]])] {
  def numMentions = mentions.length
  def features = mentions -> words

  def label = clusters

  def wordsForMention(m: (Int, Span)) = m._2 map words(m._1)
}



object CorefInstance {
  def fromDocument(doc: Document) = {
    val allMentions = {for( (s, sI) <- doc.sentences.zipWithIndex; (m, span) <- s.mentions) yield (m.id, MentionCandidate(sI, span, span map s.words))}
    val grouped = (
      allMentions.toSet[(Int, MentionCandidate)]
        .groupBy(_._1)
        .mapValues( set => set.map(_._2) )
        .values.toSet)
    CorefInstance(doc.id + "-coref",
      grouped,
      allMentions.map(_._2),
      doc.sentences.map(_.words))
  }
}

case class MentionCandidate(sentence: Int, span: Span, words: IndexedSeq[String]) extends Ordered[MentionCandidate] {
  def compare(that: MentionCandidate): Int = {
    if (this.sentence < that.sentence) -1
    else if (this.sentence > that.sentence) 1
    else if (this.span.start < that.span.start) -1
    else if (this.span.start > that.span.start) 1
    else if (this.span.end < that.span.end) -1
    else if (this.span.end > that.span.end) 1
    else 0
  }


  override def toString() = ("MC["+sentence + ":"+span.start + "-" + span.end + ", " + words.mkString(" ") + "]")
}

object MentionCandidate {
  implicit val ordering = new Ordering[MentionCandidate] {
    def compare(x: MentionCandidate, y: MentionCandidate): Int = x compare y
  }
}

