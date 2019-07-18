package epic.lexicon

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import math.log
import breeze.linalg._

/**
 * A TagScorer is just something that assigns scores to a particular
 * POS tag (or whatever) at a particular position in a sentence.
 * Uses the anchoring pattern, like most of Epic.
 * @author dlwh
 */
trait TagScorer[L, W] extends Serializable {
  def anchor(words: IndexedSeq[W]):Anchoring
  trait Anchoring {
    def words: IndexedSeq[W]
    def scoreTag(pos: Int, l: L):Double
  }
}

object TagScorer {
  /** (ab)uses Bayes' rule to go from p(t|w) to p(w|t)  via p(w) and p(t) */
  def fakeGenerativeFromDiscriminative[L, W](discriminativeTagScorer: TagScorer[L, W], counts: Counter2[L, W, Double]):TagScorer[L, W] = {
    new FakeGenerativeTagScorer(discriminativeTagScorer, counts)
  }

  @SerialVersionUID(1L)
  private class FakeGenerativeTagScorer[L, W](discriminativeTagScorer: TagScorer[L, W], counts: Counter2[L, W, Double])  extends TagScorer[L, W] with Serializable {
    val wordCounts:Counter[W, Double] = sum(counts, Axis._0)
    val total = math.log(sum(wordCounts))
    val labelCounts:Counter[L, Double] = sum(counts, Axis._1)
    breeze.numerics.log.inPlace(labelCounts)
    breeze.numerics.log.inPlace(wordCounts)
    labelCounts -= total
    wordCounts -= total

    def anchor(words: IndexedSeq[W]):Anchoring = {
      val ts = discriminativeTagScorer.anchor(words)
      // smooth at "we've seen this word once"
      val myWordCounts = words.map(w => wordCounts(w) max (-total))
      val w = words
      new Anchoring {
        def words: IndexedSeq[W] = w

        def scoreTag(pos: Int, l: L): Double = {
          (ts.scoreTag(pos, l)  + myWordCounts(pos)) - labelCounts(l)
        }
      }

    }
  }
}


/**
 * A fairly dumb tag scorer that does some simple smoothing to estimate
 * p(t|w).
 * @param counts
 * @tparam L
 * @tparam W
 */
class SimpleTagScorer[L, W](counts: Counter2[L, W, Double]) extends TagScorer[L, W] {
  def anchor(w: IndexedSeq[W]):Anchoring = new Anchoring {
    def words: IndexedSeq[W] = w

    def scoreTag(pos: Int, l: L): Double = {
      val w = words(pos)
      var cWord = wordCounts(w)
      var cTagWord = counts(l, w)
      var pTag = labelCounts(l) / totalCount
      if (pTag == 0.0) {
        pTag = 1.0
      }
      assert(cWord >= cTagWord)
      if (cWord < 10 || cTagWord == 0.0) {
        cWord += 1.0
        cTagWord += counts(l, ::).size.toDouble / wordCounts.size
        if (cTagWord == 0.0) {
          cTagWord = 1.0
        }
      }

      val pW = cWord / (totalCount + 1.0)
      val pTgW = cTagWord / cWord
      val result = log(pW) + log(pTgW) - log(pTag)
      assert(cTagWord == 0 || result > Double.NegativeInfinity)
      assert(!result.isNaN, pW + " "+  pTgW + " "+ pTag)
      assert(!result.isInfinite, pW + " "+  pTgW + " "+ pTag)
      result

    }
  }

  private val wordCounts:Counter[W, Double] = sum(counts, Axis._0)
  private val labelCounts:Counter[L, Double] = sum(counts, Axis._1)
  private val totalCount = sum(wordCounts)
}
