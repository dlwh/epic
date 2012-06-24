package epic.parser
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
    if(cWord < 10) {
      cWord += 1.0
      cTagWord += counts(l, ::).size.toDouble / wordCounts.size
    }

    val pW = cWord / (totalCount + 1.0)
    val pTgW = cTagWord / cWord
    val pTag = labelCounts(l) / totalCount
    val result = log(pW) + log(pTgW) - log(pTag)
    assert(cTagWord == 0 || result > Double.NegativeInfinity)
    result

  }

  private val wordCounts:Counter[W, Double] = sum(counts, Axis._0)
  private val labelCounts:Counter[L, Double] = sum(counts, Axis._1)
  private val totalCount = wordCounts.sum
}
