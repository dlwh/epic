package epic.lexicon

import java.io.ObjectStreamException

import breeze.linalg._
import breeze.util.Index

import scala.collection.immutable.BitSet
import scala.collection.mutable.Map

/**
 * A simple lexicon that thresholds to decide when to open up the rare word to all (open) tags
 * @param wordTagCounts (tag -> word -> count)
 * @param openTagThreshold how many different word types does a tag have to be seen with to be considered open.
 * @param closedWordThreshold How many times do we have to see a word before we decide we know its tag set?
 *                            Words with counts below this will be allowed to be any open tag in addition to their
 *                            observed tag set.
 */
@SerialVersionUID(1L)
class SimpleLexicon[L, W](
  val labelIndex: Index[L],
  wordTagCounts: Counter2[L, W, Double],
  openTagThreshold: Int = 50,
  closedWordThreshold: Int= 10
) extends Lexicon[L, W] with Serializable {
  private val wordCounts: Counter[W, Double] = sum(wordTagCounts, Axis._0)
  private val labelCounts: Counter[L, Double] = sum(wordTagCounts, Axis._1)
  private val byWord: Map[W, Set[Int]] = Map.empty[W, Set[Int]] ++
    wordTagCounts.keySet.groupBy(_._2).mapValues(_.map(pair => labelIndex(pair._1)).toSet)

  private val openTags: Set[Int] = Option(
    labelCounts.keysIterator.collect { case l if wordTagCounts(l, ::).size > openTagThreshold => labelIndex(l) }.toSet
  ).filter(_.nonEmpty).getOrElse(
    BitSet.empty ++ (0 until labelIndex.size)
  )

  for((w,v) <- wordCounts.iterator if v < closedWordThreshold)
    byWord(w) = byWord.get(w).fold(openTags)( _ ++ openTags)

  def allowedTags(w: W): Set[Int] = byWord.getOrElse(w, openTags)

  def anchor(w: IndexedSeq[W]): Anchoring = new Anchoring {
    def length = w.length
    val x = Array.tabulate(w.length)(pos => byWord.getOrElse(w(pos), openTags))
    def allowedTags(pos: Int): Set[Int] = x(pos)
  }

  @throws(classOf[ObjectStreamException])
  private def writeReplace(): Object =
    new SimpleLexicon.SerializedForm(labelIndex, wordTagCounts, openTagThreshold, closedWordThreshold)

  override def morePermissive: Lexicon[L, W] = new SimpleLexicon(labelIndex, wordTagCounts, openTagThreshold, 1000000)
}

object SimpleLexicon {

  @SerialVersionUID(1L)
  private class SerializedForm[L, W](
    labelIndex: Index[L],
    wordTagCounts: Counter2[L, W, Double],
    openTagThreshold: Int,
    closedWordThreshold: Int
  ) extends Serializable {
    @throws(classOf[ObjectStreamException])
    private def readResolve(): Object = try {
      Class.forName("breeze.linalg.Counter$Impl")
      new SimpleLexicon(labelIndex, wordTagCounts, openTagThreshold, closedWordThreshold)
    } catch {
      case ex: Throwable =>
      ex.printStackTrace()
      throw ex
    }
  }

}
