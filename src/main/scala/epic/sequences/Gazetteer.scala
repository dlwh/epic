package epic.sequences

import io.{Codec, Source}

/**
 *
 * A Gazeteer is a map from IndexedSeq[W]->L, where the second string is NER type.
 * @author dlwh
 */
trait Gazetteer[L, W] {
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
    val resource = this.getClass.getClassLoader.getResourceAsStream("ner/" + lang +".lst")
    val src = Source.fromInputStream(resource)(Codec.UTF8)
    val map: Map[IndexedSeq[String], String] = {for(line <- src.getLines()) yield {
      val arr = line.split(" " )
      arr.drop(1).toIndexedSeq -> arr(0).intern()
    }}.toMap

    val flattenedGazetteer:Map[String,IndexedSeq[String]] = {
      val justWords = for((seq, kind) <- map.toIndexedSeq; w <- seq) yield (w, kind)
      justWords.groupBy(_._1).mapValues(_.map(_._2).toSet.toIndexedSeq).toMap
    }

    val endWordsGazetteer:Map[String,IndexedSeq[String]] = {
      val justWords = for((seq, kind) <- map.toIndexedSeq; w = seq.last) yield (w, kind)
      justWords.groupBy(_._1).mapValues(_.map("END-" + _._2).toSet.toIndexedSeq).toMap
    }

    resource.close()
    new Gazetteer[String, String] {
      def lookupWord(w: String): IndexedSeq[String] = {
        flattenedGazetteer.getOrElse(w, IndexedSeq.empty) ++  endWordsGazetteer.getOrElse(w, IndexedSeq.empty)

      }

      def lookupSpan(w: IndexedSeq[String]): Option[String] = map.get(w)
    }
  }
}
