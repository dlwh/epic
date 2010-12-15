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



import scalala.tensor.counters.Counters.DoubleCounter
import scalala.tensor.counters.Counters.PairedDoubleCounter
import math.log
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter


@serializable
@SerialVersionUID(1)
trait Lexicon[L,W] {
  def wordScore(label: L, w: W): Double;
  def tagScores(w: W): LogDoubleCounter[L] = scalala.tensor.counters.LogCounters.aggregate( tags.map { l => (l,wordScore(l,w))});
  def tags: Iterator[L];

  def knownTagWords: Iterator[(L,W)]
}

class UnsmoothedLexicon[L,W](lexicon: LogPairedDoubleCounter[L,W]) extends Lexicon[L,W] {
  def wordScore(l: L, w: W) = lexicon(l,w);
  def tags = lexicon.rows.map(_._1);
  def knownTagWords = lexicon.activeKeys;
}

class SimpleLexicon[L,W](private val lexicon: PairedDoubleCounter[L,W]) extends Lexicon[L,W] {
  private val wordCounts = DoubleCounter[W]();
  lexicon.rows.foreach ( wordCounts += _._2 )
  def knownTagWords = lexicon.activeKeys;


  def wordScore(l: L, w: W) = {
    var cWord = wordCounts(w);
    var cTagWord = lexicon(l)(w);
    assert(cWord >= cTagWord);
    if(wordCounts(w) < 10 && lexicon(l).size > 50) {
      cWord += 1.0;
      cTagWord += lexicon(l).size.toDouble / wordCounts.size
    }
    if(cWord == 0) {
      Double.NegativeInfinity
    } else {
      val pW = (1.0 + cWord) / (wordCounts.total + 1.0);
      val pTgW = (cTagWord) / (cWord);
      val pTag = lexicon(l).total / wordCounts.total
      val result = log(pW) + log(pTgW) - log(pTag);
      assert(cTagWord == 0 || result > Double.NegativeInfinity)
      result;
    }
  }

  def tags = lexicon.rows.map(_._1);
}
