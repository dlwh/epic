package scalanlp.parser

import scalala.tensor.{Counter, Counter2}
import scalala.library.Library.{sum,Axis};

/**
 * 
 * @author dlwh
 */
@SerialVersionUID(1L)
class SignatureLexicon[L,W](initCounts: Counter2[L,W,Double], sigGen: W=>W,
                            threshold: Double = 2) extends Lexicon[L,W] {
  val (wordCounts, lexicon, sigCounts) = SignatureLexicon.makeSignatureCounts(sigGen, initCounts, threshold);
  def knownTagWords = initCounts.nonzero.keys.iterator
  val lexiconTotals = sum(lexicon,Axis.Vertical);

  def tags = lexicon.domain._1.iterator


  def wordScore(words: Seq[W], label: L, pos: Int):Double = wordScore(label, words(pos))

  def wordScore(l: L, w: W) = {
    val sig = if(wordCounts(w) < threshold) {
      sigGen(w);
    } else {
      w
    }

    var cWord = sigCounts(sig);
    var cTagWord = lexicon(l,sig);
    assert(cWord >= cTagWord);
    /*
    if(wordCounts(sig) < 5 && lexicon(l).size > 50) {
      cWord += 0.5;
      cTagWord += .5 * lexicon(l).size.toDouble / wordCounts.size
    }
    import math.log;
    if(cWord == 0) {
      Double.NegativeInfinity
    } else {
      val pW = (.5+ cWord) / (wordCounts.total + .5);
      val pTgW = (cTagWord) / (cWord);
      val pTag = lexicon(l).total / wordCounts.total
      val result = log(pW) + log(pTgW) - log(pTag);
      assert(cTagWord == 0 || result > Double.NegativeInfinity)
      result;
    }
    */
    import math.log
    log(cTagWord) - log(lexiconTotals(l)); // XXXX

  }
}

object SignatureLexicon {
  private def makeSignatureCounts[L,W](sigGen: W=>W,
                                       counts: Counter2[L,W, Double], threshold: Double) = {
    val wordCounts = Counter[W,Double]();
    for( ((l,w),count) <- counts.nonzero.pairs) {
      wordCounts(w) += count;
    }
    val lexicon = Counter2[L,W,Double]();
    val sigCounts = Counter[W,Double]();
    for( ((l,w),count) <- counts.nonzero.pairs) {
      val sig = if(wordCounts(w) < threshold) sigGen(w) else w

      sigCounts(sig) += count
      lexicon(l,sig) += count;
    }

    (wordCounts, lexicon, sigCounts)
  }
}
