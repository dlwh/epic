package epic.coref

import collection.immutable.BitSet
import epic.everything.{Document, DSpan}


/**
 * 
 * @author dlwh
 */
case class CorefInstance(doc: Document,
                         mentions: IndexedSeq[DSpan],
                         clusters: Set[BitSet],
                         reverseClusters: IndexedSeq[BitSet], // mention -> cluster
                         speaker: IndexedSeq[Option[String]]) extends CorefDocument {
  def words = doc.words
  def unindexedClusters: Iterable[Set[DSpan]] = clusters.map(_.map(mentions))

  def clusterFor(m: Int) = reverseClusters(m)

  def numMentions = mentions.length
}


object CorefInstance {
  class Factory(mentionDetector: Document=>IndexedSeq[DSpan]) {
    def apply(doc: Document) = {
      val mentions = mentionDetector(doc)
      val (coreferent, singletons) = (0 until mentions.length).partition(i => doc.coref.contains(mentions(i)))

      val clusters = collection.mutable.Set[BitSet]()
      clusters ++= singletons.map(BitSet(_))
      clusters ++= coreferent.groupBy(i => doc.coref(mentions(i))).values.map(BitSet.empty ++ _)


      val clusterFor = new Array[BitSet](mentions.length)
      for(c <- clusters; i <- c) {
        clusterFor(i) = c
      }

      CorefInstance(doc, mentions, clusters.toSet, clusterFor, doc.sentences.map(_.speaker))
    }

  }

  val goldMentions = {(doc: Document) => doc.coref.keys.toIndexedSeq.sorted}
}


trait CorefDocument {
  def words: IndexedSeq[IndexedSeq[String]]
  def mentions: IndexedSeq[DSpan]
  def speaker: IndexedSeq[Option[String]]
}

