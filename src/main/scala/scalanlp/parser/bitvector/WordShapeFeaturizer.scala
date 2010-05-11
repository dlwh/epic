package scalanlp.parser.bitvector

import LogisticBitVector._
import collection.mutable.ArrayBuffer;

sealed trait WordShapeFeature[+L] extends Feature[L,String];
case class IndicatorWSFeature(name: Symbol) extends WordShapeFeature[Nothing];
case class SuffixFeature(str: String) extends WordShapeFeature[Nothing];

class WordShapeFeaturizer[L] extends Featurizer[L,String] {
  def features(d: LogisticBitVector.Decision[L,String], c: LogisticBitVector.Context[L]) = d match {
    case WordDecision(w) =>
      val features = new ArrayBuffer[Feature[L,String]];
      val wlen = w.length;
      val numCaps = (w:Seq[Char]).count{_.isUpper};
      val hasLetter = w.exists(_.isLetter);
      val hasNotLetter = w.exists(!_.isLetter);
      val hasDigit = w.exists(_.isDigit);
      val hasNonDigit = hasLetter || w.exists(!_.isDigit);
      val hasLower = w.exists(_.isLower);
      val hasDash = w.contains('-');
      //TODO add INITC, KNOWNLC
      if(numCaps > 0) features += (IndicatorWSFeature('HasCap));
      if(numCaps > 1) features += (IndicatorWSFeature('HasManyCap));
      if(w(0).isUpper || w(0).isTitleCase) features += (IndicatorWSFeature('HasInitCap));
      if(hasLower) features += (IndicatorWSFeature('HasLower));
      if(hasDash) features += (IndicatorWSFeature('HasDash));
      if(hasDigit) features += (IndicatorWSFeature('HasDigit));
      if(hasLetter) features += (IndicatorWSFeature('HasLetter));
      if(hasNotLetter) features += (IndicatorWSFeature('HasNotLetter));

      if(w.length > 3 && w.endsWith("s") && !w.endsWith("ss") && !w.endsWith("us") && !w.endsWith("is"))
         features += (IndicatorWSFeature('EndsWithS));
      else if(w.length >= 5 && !hasDigit && numCaps == 0) {
        features += (SuffixFeature(w.substring(w.length-3)))
      }

      if(w.length > 10) {
        features += (IndicatorWSFeature('LongWord));
      } else if(w.length < 5) {
        features += (IndicatorWSFeature('ShortWord));
      }
      features:Seq[Feature[L,String]];

    case _ => Seq.empty;
  }

  def initFeatureWeight(logit: LogisticBitVector[L,String], f: Feature[L,String]):Option[Double] = f match {
    case f : WordShapeFeature[_] => Some(Math.log(Math.random/10));
    case _ => None;
  }
}