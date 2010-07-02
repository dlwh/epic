package scalanlp.parser.bitvector

import scala.collection.mutable.ArrayBuffer
import scalala.tensor.counters.Counters
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalanlp.parser.bitvector.LogisticBitVector.Feature
import scalanlp.parser.bitvector.LogisticBitVector.LexicalFeature
import scalanlp.parser.bitvector.LogisticBitVector.WordDecision

case class UnknownWordClass[L](l: L, clss: String) extends LogisticBitVector.Feature[L,String];

class SlavUnknownWordFeaturizer[L](lexicon: PairedDoubleCounter[L,String]) extends Featurizer[L,String] {
  val wordCounts = Counters.aggregate(for( ((_,w),count) <- lexicon.activeElements) yield (w,count));

  def features(d: LogisticBitVector.Decision[L,String], c: LogisticBitVector.Context[L]) = d match {
    case WordDecision(w) if wordCounts(w) > 5 => IndexedSeq(LexicalFeature(c._1,w));
    case WordDecision(word) =>
      val sb = new StringBuilder();

      val wlen = word.length;
      val numCaps = (word:Seq[Char]).count{_.isUpper};
      val hasLetter = word.exists(_.isLetter);
      val hasNotLetter = word.exists(!_.isLetter);
      val hasDigit = word.exists(_.isDigit);
      val hasNonDigit = hasLetter || word.exists(!_.isDigit);
      val hasLower = word.exists(_.isLower);
      val hasDash = word.contains('-');
      val l = c._1;

      val ch0 = word.charAt(0);
      val lowered = word.toLowerCase();
      if (ch0.isUpper || ch0.isTitleCase) {
        // todo location
        if (numCaps == 1) {
          sb ++= ("-INITC");
          if (wordCounts(lowered) > 0) {
            sb ++= ("-KNOWNLC");
          }
        } else {
          sb ++= ("-CAPS");
        }
      } else if (!ch0.isLetter && numCaps > 0) {
        sb ++= ("-CAPS");
      } else if (hasLower) { 
        sb ++= ("-LC");
      }
      if (hasDigit) {
        sb ++= ("-NUM");
      }
      if (hasDash) {
        sb ++= ("-DASH");
      }
      if (lowered.endsWith("s") && wlen >= 3) {
        // here length 3, so you don't miss out on ones like 80s
        val ch2 = lowered.charAt(wlen - 2);
        // not -ess suffixes or greek/latin -us, -is
        if (ch2 != 's' && ch2 != 'i' && ch2 != 'u') {
          sb ++= ("-s");
        }
      } else if (word.length() >= 5 && !hasDash && !(hasDigit && numCaps > 0)) {
        if (lowered.endsWith("ed")) {
          sb ++= ("-ed");
        } else if (lowered.endsWith("ing")) {
          sb ++= ("-ing");
        } else if (lowered.endsWith("ion")) {
          sb ++= ("-ion");
        } else if (lowered.endsWith("er")) {
          sb ++= ("-er");
        } else if (lowered.endsWith("est")) {
          sb ++= ("-est");
        } else if (lowered.endsWith("ly")) {
          sb ++= ("-ly");
        } else if (lowered.endsWith("ity")) {
          sb ++= ("-ity");
        } else if (lowered.endsWith("y")) {
          sb ++= ("-y");
        } else if (lowered.endsWith("al")) {
          sb ++= ("-al");
        }
      }


      IndexedSeq(UnknownWordClass(l,sb.toString));

    case _ => IndexedSeq.empty;
  }

  def initialValueForFeature(f: Feature[L,String]):Option[Double] = f match {
    case LexicalFeature(l,w) =>
      if(lexicon(l,w) == 0) Some(Double.NegativeInfinity)
      else Some(math.log(lexicon(l,w)) - math.log(lexicon(l).total));
    case f : UnknownWordClass[_] => Some(-30.0);
    case _ => None;
  }

  def priorForFeature(f: Feature[L,String]):Option[Double] = f match {
    case LexicalFeature(l,w) =>
      if(lexicon(l,w) == 0) Some(Double.NegativeInfinity)
      else Some(math.log(lexicon(l,w)) - math.log(lexicon(l).total));
    case f : UnknownWordClass[_] => Some(0.0);
    case _ => None;
  }
}
