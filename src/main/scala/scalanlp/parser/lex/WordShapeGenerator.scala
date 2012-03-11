package scalanlp.parser.lex

import scalala.tensor.Counter
import scalanlp.parser.epic._
import collection.mutable.ArrayBuffer
import scalanlp.text.tokenize.EnglishWordClassGenerator

/**
 * 
 * @author dlwh
 */
class WordShapeGenerator(wordCounts: Counter[String, Double], minCountUnknown: Int = 5) extends Serializable {
  val signatureGenerator = EnglishWordClassGenerator;

  def featuresFor(w: String):ArrayBuffer[Feature] = {
    if(wordCounts(w) > minCountUnknown) ArrayBuffer(IndicatorFeature(w))
    else {
      val features = ArrayBuffer[Feature]()
      features += IndicatorFeature(w);
      features += makeShapeFeature(w);
      features += IndicatorFeature(signatureGenerator.signatureFor(w));

      val wlen = w.length;
      val numCaps = (w:Seq[Char]).count{_.isUpper};
      val hasLetter = w.exists(_.isLetter);
      val hasNotLetter = w.exists(!_.isLetter);
      val hasDigit = w.exists(_.isDigit);
      val hasNonDigit = hasLetter || w.exists(!_.isDigit);
      val hasLower = w.exists(_.isLower);
      val hasDash = w.contains('-');

      if(numCaps > 0) features += (IndicatorFeature('HasCap));
      if(numCaps > 1) features += (IndicatorFeature('HasManyCap));
      if(numCaps > 1 && !hasLower) features += (IndicatorFeature('AllCaps));
      if(w(0).isUpper || w(0).isTitleCase) {
        //TODO add INITC
        features += (IndicatorFeature('HasInitCap));
        if(wordCounts(w.toLowerCase) > 3 && wordCounts(w) <= 3) {
          features += IndicatorFeature('HasKnownLC);
        }
      }
      if(!hasLower) features += (IndicatorFeature('HasNoLower));
      if(hasDash) features += (IndicatorFeature('HasDash));
      if(hasDigit) features += (IndicatorFeature('HasDigit));
      if(!hasLetter) features += (IndicatorFeature('HasNoLetter));
      if(hasNotLetter) features += (IndicatorFeature('HasNotLetter));

      if(wlen > 3 && w.endsWith("s") && !w.endsWith("ss") && !w.endsWith("us") && !w.endsWith("is"))
         features += (IndicatorFeature('EndsWithS));
      else if(wlen >= 5 && !(hasDigit && numCaps > 0) && !hasDash)  {
        features += (IndicatorFeature(w.substring(wlen-3)))
        features += (IndicatorFeature(w.substring(wlen-2)))
        features += (IndicatorFeature(w.substring(wlen-1)))
        features += IndicatorFeature(w.substring(0,1))
        features += IndicatorFeature(w.substring(0,2))
        features += IndicatorFeature(w.substring(0,3))
      }

      if(wlen > 10) {
        features += (IndicatorFeature('LongWord));
      } else if(wlen < 5) {
        features += (IndicatorFeature('ShortWord));
      }
      features
    }

  }

  def makeShapeFeature(word: String) = {
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
    new IndicatorFeature(result.toString);
  }

}