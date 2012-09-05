package epic.coref

import epic.ontonotes.{Document, Sentence, Mention}
import epic.trees.Span
import breeze.data.{Observation, Example}

/**
 *
 * @author dlwh
 */
case class CorefInstance(id: String,
                         doc: Document,
                         clusters: Set[Set[MentionCandidate]],
                         mentions: IndexedSeq[MentionCandidate],
                         words: IndexedSeq[IndexedSeq[String]]) extends CorefDocument with Example[Set[Set[MentionCandidate]], (IndexedSeq[MentionCandidate],IndexedSeq[IndexedSeq[String]])] {
  def numMentions = mentions.length

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
    CorefInstance(doc.id + "-coref", doc,
      grouped,
      allMentions.map(_._2),
      doc.sentences.map(_.words))
  }
}


trait CorefDocument extends Observation[(IndexedSeq[MentionCandidate],IndexedSeq[IndexedSeq[String]])] {
  def mentions: IndexedSeq[MentionCandidate]
  def words: IndexedSeq[IndexedSeq[String]]

  def features = mentions -> words
}

object CorefDocument {
  def apply(id: String, mentions: IndexedSeq[MentionCandidate], words: IndexedSeq[IndexedSeq[String]]):CorefDocument = {
    val i = id
    val m = mentions
    val w = words
    new CorefDocument {
      def mentions = m
      def words = w
      def id = i
    }
  }
}