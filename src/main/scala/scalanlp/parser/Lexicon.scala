package scalanlp.parser

import scalanlp.counters.Counters.DoubleCounter
import scalanlp.counters.Counters.PairedDoubleCounter
import Math.log
import scalanlp.counters.LogCounters.LogPairedDoubleCounter


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