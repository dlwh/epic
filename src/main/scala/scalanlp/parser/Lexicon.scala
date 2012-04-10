package scalanlp.parser
/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
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
import scalala.tensor.{Counter2, Counter}
import scalala.tensor.::
import scalala.library.Library._;

/**
 * Scores (label, word) pairs in a sentence
 */
@SerialVersionUID(1)
trait Lexicon[L, W] extends Serializable {
  def wordScore(words: Seq[W], label: L, pos: Int): Double;
  @deprecated
  def tagScores(w: W): Counter[L, Double] = Counter( tags.map { l => (l, wordScore(Seq(w), l, 0))});
  def tags: Iterator[L];

  @deprecated
  def knownTagWords: Iterator[(L, W)]
}

object Lexicon {
  // TODO Probably delete this implicit soon
  implicit def counterToLexicon[L, W](wordCounts: Counter2[L, W, Double]) = new SimpleLexicon(wordCounts)
}

// counter should be in log space
class UnsmoothedLexicon[L, W](lexicon: Counter2[L, W, Double]) extends Lexicon[L, W] {
  def wordScore(words: Seq[W], l: L, pos: Int) = {
    if(lexicon.contains(l, words(pos))) lexicon(l, words(pos))  else Double.NegativeInfinity
  }
  def tags = lexicon.domain._1.iterator
  def knownTagWords = lexicon.nonzero.keys.iterator;
}

// counter should be in normal space
class SimpleLexicon[L, W](private val lexicon: Counter2[L, W, Double]) extends Lexicon[L, W] {
  private val wordCounts:Counter[W, Double] = sum(lexicon)
  private val labelCounts:Counter[L, Double] = sum(lexicon, Axis.Vertical)
  private val totalCount = wordCounts.sum
  def knownTagWords = lexicon.nonzero.keys.iterator;

  def wordScore(words: Seq[W], l: L, pos: Int) = {
    val w = words(pos)
    var cWord = wordCounts(w);
    var cTagWord = lexicon(l, w);
    assert(cWord >= cTagWord);
    if(wordCounts(w) < 10 && lexicon(l, ::).size > 50) {
      cWord += 1.0;
      cTagWord += lexicon(l, ::).size.toDouble / wordCounts.size
    }
    if(cWord == 0) {
      Double.NegativeInfinity
    } else {
      val pW = (1.0 + cWord) / (totalCount + 1.0)
      val pTgW = (cTagWord) / (cWord);
      val pTag = labelCounts(l) / totalCount
      val result = log(pW) + log(pTgW) - log(pTag);
      assert(cTagWord == 0 || result > Double.NegativeInfinity)
      result
    }
  }

  def tags = lexicon.domain._1.iterator;
}
