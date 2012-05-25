package scalanlp.parser

import math.log
import scalala.tensor.{Counter, Counter2, ::}
import scalala.library.Library.{sum,Axis}

/**
 * TODO
 * @author dlwh
 */
trait TagScorer[L, W] extends Serializable {
  def scoreTag(l: L, words: Seq[W], pos: Int):Double
}

class SimpleTagScorer[L, W](counts: Counter2[L, W, Double]) extends TagScorer[L, W] {
  def scoreTag(l: L, words: Seq[W], pos: Int) = {
    val w = words(pos)
    var cWord = wordCounts(w)
    var cTagWord = counts(l, w)
    assert(cWord >= cTagWord)
    if(wordCounts(w) < 10) {
      cWord += 1.0
      cTagWord += counts(l, ::).size.toDouble / wordCounts.size
    }
    if(cWord == 0) {
      Double.NegativeInfinity
    } else {
      val pW = cWord / (totalCount + 1.0)
      val pTgW = cTagWord / cWord
      val pTag = labelCounts(l) / totalCount
      val result = log(pW) + log(pTgW) - log(pTag)
      assert(cTagWord == 0 || result > Double.NegativeInfinity)
      result
    }

  }

  private val wordCounts:Counter[W, Double] = sum(counts)
  private val labelCounts:Counter[L, Double] = sum(counts, Axis.Vertical)
  private val totalCount = wordCounts.sum
}
