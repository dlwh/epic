package scalanlp.parser
package bitvector

import LogisticBitVector._
import scalala.tensor.counters.Counters
import scalala.tensor.counters.Counters.PairedDoubleCounter
import collection.mutable.ArrayBuffer;


sealed abstract class WordShapeFeature[+L](l: L) extends Feature[L,String];
final case class IndicatorWSFeature[L](l: L, name: Symbol) extends WordShapeFeature(l);
final case class SuffixFeature[L](l: L, str: String) extends WordShapeFeature(l);

class WordShapeFeaturizer[L](lexicon: PairedDoubleCounter[L,String]) extends Featurizer[L,String] {
  val wordCounts = Counters.aggregate(for( ((_,w),count) <- lexicon.activeElements) yield (w,count));

  def features(d: LogisticBitVector.Decision[L,String], c: LogisticBitVector.Context[L]) = d match {
    case WordDecision(w) if wordCounts(w) > 5 => IndexedSeq(LexicalFeature(c._1,w));
    case WordDecision(w) =>
      val features = new ArrayBuffer[Feature[L,String]];

      features += LexicalFeature(c._1,w);
      if(wordCounts(w) > 3) {
       features += IndicatorWSFeature(c._1,'Uncommon);
      } else if(wordCounts(w) > 1) {
      } else {
        features += IndicatorWSFeature(c._1,'Unknown);
      }

      val wlen = w.length;
      val numCaps = (w:Seq[Char]).count{_.isUpper};
      val hasLetter = w.exists(_.isLetter);
      val hasNotLetter = w.exists(!_.isLetter);
      val hasDigit = w.exists(_.isDigit);
      val hasNonDigit = hasLetter || w.exists(!_.isDigit);
      val hasLower = w.exists(_.isLower);
      val hasDash = w.contains('-');
      val l = c._1;
      if(numCaps > 0) features += (IndicatorWSFeature(l,'HasCap));
      if(numCaps > 1) features += (IndicatorWSFeature(l,'HasManyCap));
      if(numCaps > 1 && !hasLower) features += (IndicatorWSFeature(l,'AllCaps));
      if(w(0).isUpper || w(0).isTitleCase) {
        //TODO add INITC
        features += (IndicatorWSFeature(l,'HasInitCap));
        if(wordCounts(w.toLowerCase) > 3 && wordCounts(w) <= 3) {
          features += IndicatorWSFeature(l,'HasKnownLC);
          if(lexicon(l,w.toLowerCase) > 0)
            features += LexicalFeature(l,w.toLowerCase);
        }
      }
      if(!hasLower) features += (IndicatorWSFeature(l,'HasNoLower));
      if(hasDash) features += (IndicatorWSFeature(l,'HasDash));
      if(hasDigit) features += (IndicatorWSFeature(l,'HasDigit));
      if(!hasLetter) features += (IndicatorWSFeature(l,'HasNoLetter));
      if(hasNotLetter) features += (IndicatorWSFeature(l,'HasNotLetter));

      if(w.length > 3 && w.endsWith("s") && !w.endsWith("ss") && !w.endsWith("us") && !w.endsWith("is"))
         features += (IndicatorWSFeature(l,'EndsWithS));
      else if(w.length >= 5 && !(hasDigit && numCaps > 0) && !hasDash)  {
        features += (SuffixFeature(l,w.substring(w.length-3)))
        features += (SuffixFeature(l,w.substring(w.length-2)))
      }

      if(w.length > 10) {
        features += (IndicatorWSFeature(l,'LongWord));
      } else if(w.length < 5) {
        features += (IndicatorWSFeature(l,'ShortWord));
      }
      features:IndexedSeq[Feature[L,String]];

    case _ => IndexedSeq.empty;
  }

  def initialValueForFeature(f: Feature[L,String]):Option[Double] = f match {
    case LexicalFeature(l,w) => 
      if(lexicon(l,w) == 0) Some(Double.NegativeInfinity)
      else Some(math.log(lexicon(l,w)) - math.log(lexicon(l).total));
    case f : WordShapeFeature[_] => Some(-30.0);
    case _ => None;
  }

  def priorForFeature(f: Feature[L,String]):Option[Double] = f match {
    case LexicalFeature(l,w) =>
      if(lexicon(l,w) == 0) Some(Double.NegativeInfinity)
      else Some(math.log(lexicon(l,w)) - math.log(lexicon(l).total));
    case f : WordShapeFeature[_] => Some(0.0);
    case _ => None;
  }
}