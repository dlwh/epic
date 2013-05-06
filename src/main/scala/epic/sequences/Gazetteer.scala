package epic.sequences

import io.{Codec, Source}

/**
 *
 * A Gazeteer is a map from IndexedSeq[W]->L. That is, it maps strings of words
 * to a label that we've seen before. For example, you might use a list of countries.
 * These are very useful for named entity recognition.
 * @author dlwh
 */
trait Gazetteer[+L, -W] {
  def lookupWord(w: W):IndexedSeq[L]
  def lookupSpan(w: IndexedSeq[W]):Option[L]
}

object Gazetteer {
  def empty[L, W]:Gazetteer[L, W] = new Gazetteer[L, W] {
    def lookupWord(w: W): IndexedSeq[L] = IndexedSeq.empty

    def lookupSpan(w: IndexedSeq[W]): Option[L] = None
  }

  /**
   * Returns the gazetteer for a given language (just english right now).
   *
   *
   * @param lang
   * @return
   */
  def ner(lang: String="en"):Gazetteer[String, String] = {
    val resource = this.getClass.getClassLoader.getResourceAsStream(s"ner/$lang.lst")
    val src = Source.fromInputStream(resource)(Codec.UTF8)
    val map: Map[IndexedSeq[String], String] = {for(line <- src.getLines()) yield {
      val arr = line.split(" " )
      arr.drop(1).toIndexedSeq -> arr(0).intern()
    }}.toMap

    val flattenedGazetteer:Map[String,IndexedSeq[String]] = {
      val justWords = for((seq, kind) <- map.toIndexedSeq; w <- seq) yield (w, kind)
      justWords.groupBy(_._1).map{ case (k,v) => k -> v.map(_._2).toSet.toIndexedSeq}
    }

    val endWordsGazetteer:Map[String,IndexedSeq[String]] = {
      val justWords = for((seq, kind) <- map.toIndexedSeq; w = seq.last) yield (w, kind)
      justWords.groupBy(_._1).map{ case (k, v) => k -> v.map("END-" + _._2).toSet.toIndexedSeq}
    }

    resource.close()
    new SimpleGazetteer(flattenedGazetteer, endWordsGazetteer, map)
  }

  @SerialVersionUID(1L)
  final class SimpleGazetteer[L, W](flattenedGazetteer: Map[W, IndexedSeq[L]], endWords: Map[W, IndexedSeq[L]], spanMap: Map[IndexedSeq[W], L]) extends Gazetteer[L, W] with Serializable {
    def lookupWord(w: W): IndexedSeq[L] = {
      flattenedGazetteer.getOrElse(w, IndexedSeq.empty) ++  endWords.getOrElse(w, IndexedSeq.empty)

    }

    def lookupSpan(w: IndexedSeq[W]): Option[L] = spanMap.get(w)
  }
}
