package scalanlp.parser

import scalala.tensor.{Counter, Counter2}
import scalala.library.Library.{sum,Axis};

/**
 * 
 * @author dlwh
 */
@SerialVersionUID(1L)
class SignatureLexicon[L,W](initCounts: Counter2[L,W,Double], sigGen: SignatureGenerator[W],
                            threshold: Double = 2) extends Lexicon[L,W] {
  val (wordCounts, lexicon, sigCounts) = SignatureLexicon.makeSignatureCounts(sigGen, initCounts, threshold);
  def knownTagWords = initCounts.nonzero.keys.iterator
  val lexiconTotals = sum(lexicon,Axis.Vertical);

  def tags = lexicon.domain._1.iterator

  def wordScore(l: L, w: W) = {
    val sig = if(wordCounts(w) < threshold) {
      sigGen.signatureFor(w);
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
  private def makeSignatureCounts[L,W](sigGen: SignatureGenerator[W],
                                       counts: Counter2[L,W, Double], threshold: Double) = {
    val wordCounts = Counter[W,Double]();
    for( ((l,w),count) <- counts.nonzero.pairs) {
      wordCounts(w) += count;
    }
    val lexicon = Counter2[L,W,Double]();
    val sigCounts = Counter[W,Double]();
    for( ((l,w),count) <- counts.nonzero.pairs) {
      val sig = if(wordCounts(w) < threshold) sigGen.signatureFor(w) else w

      sigCounts(sig) += count
      lexicon(l,sig) += count;
    }

    (wordCounts, lexicon, sigCounts)
  }
}

@serializable
trait SignatureGenerator[W] {
  def signatureFor(w: W):W
}

object SignatureGenerator {
  def unknown[W](unknown: W): SignatureGenerator[W] = new ConstantSigGenerator(unknown);
  @serializable
  @SerialVersionUID(1)
  class ConstantSigGenerator[W](unknown: W) extends SignatureGenerator[W] {
    def signatureFor(w: W) = unknown;
  }
}

class UnknownSigLexicon[L](initCounts: Counter2[L,String,Double],
                           threshold: Double = 2) extends SignatureLexicon(initCounts,SignatureGenerator.unknown(">>UNKNOWN<<"),threshold);

@SerialVersionUID(1L)
object EnglishWordClassGenerator extends SignatureGenerator[String] {
  def signatureFor(word: String) = {
    val sb = new StringBuilder;
    val wlen = word.length();
    val numCaps = (word:Seq[Char]).count(_.isUpper);
    val hasDigit = word.exists(_.isDigit);
    val hasDash = word.contains('-');
    val hasLower = numCaps < wlen;
    val ch0 = word.charAt(0);
    val lowered = word.toLowerCase();
    if (Character.isUpperCase(ch0) || Character.isTitleCase(ch0)) {
      if (numCaps == 1) {
        sb.append("-INITC");
      } else {
        sb.append("-CAPS");
      }
    } else if (!Character.isLetter(ch0) && numCaps > 0) {
      sb.append("-CAPS");
    } else if (hasLower) {
      sb.append("-LC");
    }

    if (hasDigit) {
      sb.append("-NUM");
    }
    if (hasDash) {
      sb.append("-DASH");
    }
    if (lowered.endsWith("s") && wlen >= 3) {
      // here length 3, so you don't miss out on ones like 80s
      val ch2 = lowered.charAt(wlen - 2);
      // not -ess suffixes or greek/latin -us, -is
      if (ch2 != 's' && ch2 != 'i' && ch2 != 'u') {
        sb.append("-s");
      }
    } else if (word.length() >= 5 && !hasDash && !(hasDigit && numCaps > 0)) {
      if (lowered.endsWith("ed")) {
        sb.append("-ed");
      } else if (lowered.endsWith("ing")) {
        sb.append("-ing");
      } else if (lowered.endsWith("ion")) {
        sb.append("-ion");
      } else if (lowered.endsWith("er")) {
        sb.append("-er");
      } else if (lowered.endsWith("est")) {
        sb.append("-est");
      } else if (lowered.endsWith("ly")) {
        sb.append("-ly");
      } else if (lowered.endsWith("ity")) {
        sb.append("-ity");
      } else if (lowered.endsWith("y")) {
        sb.append("-y");
      } else if (lowered.endsWith("al")) {
        sb.append("-al");
      }
    }
    sb.toString;
  }
}
