package epic.lexicon

import breeze.linalg._
import java.io.ObjectStreamException
import breeze.util.Index
import epic.trees.LexicalProduction

/**
 * A simple lexicon that thresholds to decide when to open up the rare word to all (open) tags
 * @param wordTagCounts (tag -> word -> count)
 * @param openTagThreshold how many different word types does a tag have to be seen with to be considered open.
 * @param closedWordThreshold How many
 */
class SimpleLexicon[L, W](val labelIndex: Index[L],
                          wordTagCounts: Counter2[L, W, Double],
                          openTagThreshold: Int = 50,
                          closedWordThreshold: Int= 10) extends Lexicon[L, W] {
  private val wordCounts:Counter[W, Double] = sum(wordTagCounts, Axis._0)
  private val labelCounts:Counter[L, Double] = sum(wordTagCounts, Axis._1)

  private val byWord = collection.mutable.Map.empty[W, Set[Int]] ++ wordTagCounts.keySet.groupBy(_._2).mapValues(_.map(pair => labelIndex(pair._1)))

  private val openTags = labelCounts.keysIterator.filter(l => wordTagCounts(l, ::).size > openTagThreshold).toSet.map((l:L) => labelIndex(l))

  for( (w,v) <- wordCounts.iterator if v < closedWordThreshold) {
    byWord.get(w) match {
      case None => byWord(w) = collection.mutable.Set() ++= openTags
      case Some(set) => byWord(w) = set ++ openTags
    }
  }

  def knownLexicalProductions = for( (w,set) <- byWord.iterator; l <- set.iterator) yield LexicalProduction(labelIndex.get(l), w)

  def anchor(w: IndexedSeq[W]) = new Localization {
    def tagsForWord(pos: Int): Set[Int] = byWord.getOrElse(w(pos), Set.empty).toSet
  }


  @throws(classOf[ObjectStreamException])
  private def writeReplace():Object = {
    new SimpleLexicon.SerializedForm(labelIndex, wordTagCounts, openTagThreshold, closedWordThreshold)
  }
}

object SimpleLexicon {
  @SerialVersionUID(1L)
  private class SerializedForm[L, W](labelIndex: Index[L], wordTagCounts: Counter2[L, W, Double], openTagThreshold: Int = 50, closedWordThreshold: Int= 10) extends Serializable {
    @throws(classOf[ObjectStreamException])
    protected def readResolve():Object = {
      new SimpleLexicon(labelIndex, wordTagCounts, openTagThreshold, closedWordThreshold)
    }
  }
}
