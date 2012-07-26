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

case class MentionCandidate(sentence: Int, span: Span, words: IndexedSeq[String])