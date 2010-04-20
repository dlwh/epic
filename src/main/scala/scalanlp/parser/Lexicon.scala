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
import Math.log
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter


trait Lexicon[L,W] {
  def wordScore(label: L, w: W): Double;
  def tags: Iterator[L];
}

class UnsmoothedLexicon[L,W](lexicon: LogPairedDoubleCounter[L,W]) extends Lexicon[L,W] {
  def wordScore(l: L, w: W) = lexicon(l,w);
  def tags = lexicon.rows.map(_._1);
}


class SimpleLexicon[L,W](private val lexicon: PairedDoubleCounter[L,W]) extends Lexicon[L,W] {
  private val wordCounts = DoubleCounter[W]();
  lexicon.rows.foreach ( wordCounts += _._2 )

  def wordScore(l: L, w: W) = {
    var cWord = wordCounts(w);
    var cTagWord = lexicon(l)(w);
    if(wordCounts(w) < 4) {
      cWord += 1.0;
      cTagWord += lexicon(l).size.toDouble / wordCounts.size
    }
    val pW = (1.0 + cWord) / (wordCounts.total + 1.0);
    val pTgW = (cTagWord) / (cWord);
    val pTag = lexicon(l).total / wordCounts.total
    val result = log(pW * pTgW / pTag);
    result;
  }

  def tags = lexicon.rows.map(_._1);
}