package epic.parser.features

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import epic.framework.Feature
import breeze.linalg._
import collection.mutable.ArrayBuffer
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}

final case class IndicatorWSFeature(name: Symbol) extends Feature
final case class SuffixFeature(str: String) extends Feature
final case class PrefixFeature(str: String) extends Feature
final case class ShapeFeature(str: String) extends Feature
final case class SignatureFeature(str: String) extends Feature
final case class SeenWithTagFeature(str: Any) extends Feature
final case class LeftWordFeature(str: Any) extends Feature
final case class RightWordFeature(str: Any) extends Feature



class WordShapeFeaturizer(wordCounts: Counter[String, Double], minCountUnknown: Int = 5) extends ((Seq[String],Int)=>IndexedSeq[Feature]) {
  import WordShapeFeaturizer._
  val signatureGenerator = EnglishWordClassGenerator
  def featuresFor(words: Seq[String], pos: Int): IndexedSeq[Feature] = {
    val w = words(pos)
    val wc = wordCounts(w)
    val features = ArrayBuffer[Feature]()
    if(wc > minCountUnknown) {
      ArrayBuffer(IndicatorFeature(w))
    } else {
      features += IndicatorFeature(w)
      features += ShapeFeature(WordShapeGenerator(w))
      features += SignatureFeature(signatureGenerator.signatureFor(w))

      val wlen = w.length
      val numCaps = (w:Seq[Char]).count{_.isUpper}
      val hasLetter = w.exists(_.isLetter)
      val hasNotLetter = w.exists(!_.isLetter)
      val hasDigit = w.exists(_.isDigit)
      val hasNonDigit = hasLetter || w.exists(!_.isDigit)
      val hasLower = w.exists(_.isLower)
      val hasDash = w.contains('-')

      if(numCaps > 0)  features += hasCapFeature
      if(numCaps > 1)  features += hasManyCapFeature
      if(numCaps > 1 && !hasLower) features += isAllCapsFeature
      if(w(0).isUpper || w(0).isTitleCase) {
        if(pos == 0) {
          features += startOfSentenceFeature
        } else {
          features += hasInitCapFeature
          if(wordCounts(w.toLowerCase) > 3) {
            features += hasKnownLCFeature
          }
        }
      }

      if(!hasLower) features += hasNoLower
      if(hasDash) features += hasDashFeature
      if(hasDigit)  features += hasDigitFeature
      if(!hasLetter)  features += hasNoLetterFeature
      if(hasNotLetter)  features += hasNotLetterFeature

      if(wlen > 3 && w.endsWith("s") && !w.endsWith("ss") && !w.endsWith("us") && !w.endsWith("is"))
        features += endsWithSFeature
      else if(wlen >= 5 && !(hasDigit && numCaps > 0) && !hasDash)  {
        features += (SuffixFeature(w.substring(wlen-3)))
        features += (SuffixFeature(w.substring(wlen-2)))
        features += (SuffixFeature(w.substring(wlen-1)))
        features += PrefixFeature(w.substring(0,1))
        features += PrefixFeature(w.substring(0,2))
        features += PrefixFeature(w.substring(0,3))

      }

      if(wlen > 10) {
        features += longWordFeature
      } else if(wlen < 5) {
        features += shortWordFeature
      }
      features
    }
  }

  def apply(v1: Seq[String], v2: Int) = featuresFor(v1,v2)


}

object WordShapeFeaturizer {

  // features
  val hasNoLower = IndicatorWSFeature('HasNoLower)
  val hasDashFeature = IndicatorWSFeature('HasDash)
  val hasDigitFeature = IndicatorWSFeature('HasDigit)
  val hasNoLetterFeature = IndicatorWSFeature('HasNoLetter)
  val hasNotLetterFeature = IndicatorWSFeature('HasNotLetter)
  val endsWithSFeature = IndicatorWSFeature('EndsWithS)
  val longWordFeature = IndicatorWSFeature('LongWord)
  val shortWordFeature = IndicatorWSFeature('ShortWord)
  val hasKnownLCFeature = IndicatorWSFeature('HasKnownLC)
  val hasInitCapFeature = IndicatorWSFeature('HasInitCap)
  val hasCapFeature = IndicatorWSFeature('HasCap)
  val hasManyCapFeature = IndicatorWSFeature('HasManyCap)
  val isAllCapsFeature = IndicatorWSFeature('AllCaps)
  val startOfSentenceFeature = IndicatorWSFeature('StartOfSentence)
}

/**
 * 
 * @author dlwh
 */
class TagAwareWordShapeFeaturizer[L](tagWordCounts: Counter2[L, String, Double], minCountUnknown: Int = 5) extends ((Seq[String], Int)=>IndexedSeq[Feature]) with Serializable {

  val wordCounts = sum(tagWordCounts, Axis._0)

  val tagDiversity: Counter[String, Int] = {
     Counter(tagWordCounts.keySet.groupBy(_._2).mapValues(_.size))
  }

  private val featurizer = new WordShapeFeaturizer(wordCounts, minCountUnknown)

  def apply(w: Seq[String], pos: Int) = featuresFor(w, pos)

  def featuresFor(words: Seq[String], pos: Int):ArrayBuffer[Feature] = {
    val w = words(pos)
    val wc = wordCounts(w)
    val features = ArrayBuffer[Feature]()
    features ++= featurizer.featuresFor(words, pos)
    val basicFeatures = if(wc > minCountUnknown) {
      ArrayBuffer(IndicatorFeature(w):Feature)
    } else {

      for( (l,v) <- tagWordCounts(::, w).iterator) {
        if(v > 1) {
          features += SeenWithTagFeature(l)
        }

      }

      features
    }

    // if tag is not super common, or if it's ambiguous, add context features of left and right word
    // contexts are the word itself if it's common, and the word shape otherwise.
    if(wc < 30 || tagDiversity(w) > 1) {
      if (pos > 0) {
        val prevWord: String = words(pos - 1)
        if(wordCounts(prevWord) > 30)
          basicFeatures  += LeftWordFeature(prevWord)
        else
          basicFeatures  += LeftWordFeature(WordShapeGenerator(prevWord))
      }

      if(pos < words.length - 1 && wordCounts(words(pos+1)) > 30) {
        val rightWord: String = words(pos + 1)
        if(wordCounts(rightWord) > 30)
          basicFeatures  += RightWordFeature(rightWord)
        else
          basicFeatures  += RightWordFeature(WordShapeGenerator(rightWord))
      }
    }

    basicFeatures

  }

}


