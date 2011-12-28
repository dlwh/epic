package scalanlp.parser
package discrim

import collection.mutable.ArrayBuffer
import scalala.tensor.{Counter2, Counter}
import scalala.library.Library._

sealed abstract class WordShapeFeature[+L](l: L) extends Feature[L,Nothing];
final case class IndicatorWSFeature[L](l: L, name: Symbol) extends WordShapeFeature(l);
final case class SuffixFeature[L](l: L, str: String) extends WordShapeFeature(l);
final case class PrefixFeature[L](l: L, str: String) extends WordShapeFeature(l);
final case class ShapeFeature[+L](l: L, str: String) extends WordShapeFeature(l);
final case class SignatureFeature[+L](l: L, str: String) extends WordShapeFeature(l);

/**
 * This class generates features according to the word shapes.
 */
class WordShapeFeaturizer[L](lexicon: Counter2[L,String,Double], initToZero: Boolean = true, scale: Double = 1.0) extends Featurizer[L,String] {
  val wordCounts = sum(lexicon,Axis.Horizontal)
  val labelTotals = sum(lexicon,Axis.Vertical)

  val signatureGenerator = EnglishWordClassGenerator;


  def initialValueForFeature(f: Feature[L,String]) = f match {
    case LexicalFeature(l,w:String) if !initToZero =>
      val r = math.log(lexicon(l,w)/labelTotals(l)) /scale;
      if(r == Double.NegativeInfinity) -30
      else r
    case _ => 0.0;
  }

  def featuresFor(r: Rule[L]) = Counter[Feature[L,String],Double]();

  def featuresFor(l: L, w: String) = {
    if(wordCounts(w) > 5) Counter(LexicalFeature(l,w) -> 1.0);
    else {
      val features = ArrayBuffer[Feature[L,String]]();
      features += LexicalFeature(l,w);
      features += makeShapeFeature(l, w);
      features += SignatureFeature(l,signatureGenerator.signatureFor(w));

      val wlen = w.length;
      val numCaps = (w:Seq[Char]).count{_.isUpper};
      val hasLetter = w.exists(_.isLetter);
      val hasNotLetter = w.exists(!_.isLetter);
      val hasDigit = w.exists(_.isDigit);
      val hasNonDigit = hasLetter || w.exists(!_.isDigit);
      val hasLower = w.exists(_.isLower);
      val hasDash = w.contains('-');

      if(numCaps > 0) features += (IndicatorWSFeature(l,'HasCap));
      if(numCaps > 1) features += (IndicatorWSFeature(l,'HasManyCap));
      if(numCaps > 1 && !hasLower) features += (IndicatorWSFeature(l,'AllCaps));
      if(w(0).isUpper || w(0).isTitleCase) {
        //TODO add INITC
        features += (IndicatorWSFeature(l,'HasInitCap));
        if(wordCounts(w.toLowerCase) > 3 && wordCounts(w) <= 3) {
          features += IndicatorWSFeature(l,'HasKnownLC);
          if(lexicon(l,w.toLowerCase) > 0)
            features += IndicatorWSFeature(l,'WordOccursAsLCWithTag)
          else features += IndicatorWSFeature(l,'WordDoesNotOccurAsLCWithTag)
        }
      }
      if(!hasLower) features += (IndicatorWSFeature(l,'HasNoLower));
      if(hasDash) features += (IndicatorWSFeature(l,'HasDash));
      if(hasDigit) features += (IndicatorWSFeature(l,'HasDigit));
      if(!hasLetter) features += (IndicatorWSFeature(l,'HasNoLetter));
      if(hasNotLetter) features += (IndicatorWSFeature(l,'HasNotLetter));

      if(hasDash && (hasLetter || hasDigit))  {
        val split = w.split("-");
        val lastInSplit = split.last;
        if(lexicon(l,lastInSplit) > 0) {
          features += IndicatorWSFeature(l,'PostHyphenOccursWithLabel)
        } else if(wordCounts(lastInSplit) > 0) {
          features += IndicatorWSFeature(l,'PostHyphenDoesNotOccurWithLabel)
        }
      }



      if(w.length > 3 && w.endsWith("s") && !w.endsWith("ss") && !w.endsWith("us") && !w.endsWith("is"))
         features += (IndicatorWSFeature(l,'EndsWithS));
      else if(w.length >= 5 && !(hasDigit && numCaps > 0) && !hasDash)  {
        features += (SuffixFeature(l,w.substring(w.length-3)))
        features += (SuffixFeature(l,w.substring(w.length-2)))
        features += (SuffixFeature(l,w.substring(w.length-1)))
        features += PrefixFeature(l,w.substring(0,1))
        features += PrefixFeature(l,w.substring(0,2))
        features += PrefixFeature(l,w.substring(0,3))
      }

      if(w.length > 10) {
        features += (IndicatorWSFeature(l,'LongWord));
      } else if(w.length < 5) {
        features += (IndicatorWSFeature(l,'ShortWord));
      }
      Counter(features.iterator.map(f => (f,1.0)));
    }

  }

  def makeShapeFeature(l: L, word: String) = {
    val result = new StringBuilder(word.length);
    var i = 0;
    while(i < word.length) {
      val c = word(i);
      val x = if(c.isLetter && c.isUpper) 'X' else if(c.isLetter) 'x' else if(c.isDigit) 'd' else c;
      if(result.length > 1 && (result.last == x) && result(result.length - 2) == x) {
        result += 'e'
      } else if (result.length > 1 && result.last == 'e' && result(result.length - 2) == x) {
        () // nothing
      }else {
        result += x;
      }
      i +=1;
    }
    new ShapeFeature(l,result.toString);
  }

}