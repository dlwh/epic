package epic.features

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
import breeze.linalg._
import breeze.util.{Encoder, Index}
import epic.framework.Feature

import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(1L)
final case class IndicatorWSFeature(name: Symbol) extends Feature
@SerialVersionUID(1L)
final case class SuffixFeature(str: String) extends Feature
@SerialVersionUID(1L)
final case class PrefixFeature(str: String) extends Feature
@SerialVersionUID(1L)
final case class ShapeFeature(str: String) extends Feature
@SerialVersionUID(1L)
final case class SignatureFeature(str: String) extends Feature
@SerialVersionUID(1L)
final case class SeenWithTagFeature(str: Any) extends Feature
@SerialVersionUID(1L)
final case class LeftWordFeature(str: Any) extends Feature
@SerialVersionUID(1L)
final case class RightWordFeature(str: Any) extends Feature

@SerialVersionUID(1L)
class WordPropertyFeaturizer(wordCounts: Counter[String, Double],
                             commonWordThreshold: Int = 20) extends WordFeaturizer[String] with Serializable {
  import epic.features.WordPropertyFeaturizer._

  private val wordIndex = Index(wordCounts.keysIterator)
  private val knownWordFeatures = Encoder.fromIndex(wordIndex).tabulateArray(s => featuresFor(s).toArray)

  def anchor(w: IndexedSeq[String]): WordFeatureAnchoring[String] = new WordFeatureAnchoring[String] {
    def words: IndexedSeq[String] = w
    val indices = words.map(wordIndex)
    val myFeatures = words.indices.map(i => if (indices(i) < 0) featuresFor(words(i)).toArray else knownWordFeatures(indices(i)))
    def featuresForWord(pos: Int): Array[Feature] = {
      if (pos < 0) Array(BeginSentFeature)
      else if (pos >= words.length) Array(EndSentFeature)
      else {
      val base = myFeatures(pos)
        // initial words nee special treatment
        if ( (words(pos).charAt(0).isUpper || words(pos).charAt(0).isTitleCase) && base.length > 1) {
          val isInitialWord = pos == 0 || words(pos -1) == "``"
          if (isInitialWord) {
            base ++ base.map(FirstWordCapsAnd)
          } else {
            base ++ base.map(NthWordCapsAnd)
          }
        } else {
          base
        }
      }
    }
  }

  //  val signatureGenerator = EnglishWordClassGenerator
  def featuresFor(w: String): IndexedSeq[Feature] = {
    val wc = wordCounts(w)
    val features = ArrayBuffer[Feature]()
    if (wc <= commonWordThreshold) {
      val wlen = w.length
      val numCaps = (w:Seq[Char]).count{_.isUpper}
      val hasLetter = w.exists(_.isLetter)
      val hasNotLetter = w.exists(!_.isLetter)
      val hasDigit = w.exists(_.isDigit)
      val hasNonDigit = hasLetter || w.exists(!_.isDigit)
      val hasLower = w.exists(_.isLower)
      val hasDash = w.contains('-')
      val numPeriods = w.count('.' ==)
      val hasPeriod = numPeriods > 0

      if (numCaps > 0)  features += hasCapFeature
      if (numCaps > 1)  features += hasManyCapFeature
      val isAllCaps = numCaps > 1 && !hasLower && !hasNotLetter
      if (isAllCaps) features += isAllCapsFeature

      if (w.length == 2 && w(0).isLetter && w(0).isUpper && w(1) == '.') {
        features += isAnInitialFeature
      }

      if (w.length > 1 && w.last == '.') {
        features += endsWithPeriodFeature

      }

      var knownLowerCase = false
      var hasTitleCaseVariant = false

      val hasInitialUpper: Boolean = w(0).isUpper || w(0).isTitleCase
      if (hasInitialUpper) {
        features += hasInitCapFeature
        if (wordCounts(w.toLowerCase) > 0) {
          features += hasKnownLCFeature
          knownLowerCase = true
        } else {
          hasTitleCaseVariant = wordCounts(w(0).toTitleCase + w.substring(1).toLowerCase) > 0
          if (isAllCaps && hasTitleCaseVariant) {
            features += hasKnownTitleCaseFeature
          }
        }
      }

      if (!hasLower && hasLetter) features += hasNoLower
      if (hasDash) features += hasDashFeature
      if (hasDigit) {
        features += hasDigitFeature
        features += DigitNormalizedFeature(w.replaceAll("\\d", "0"))
      }
      if (!hasLetter)  features += hasNoLetterFeature
      if (hasNotLetter)  features += hasNotLetterFeature

      // acronyms are all upper case with maybe some periods interspersed
      val hasAcronymShape = (
        wlen >= 3 && isAllCaps && wlen < 6
        || wlen >= 2 && hasPeriod && !hasLower && numCaps > 0 && !hasDigit && w.forall(c => c.isLetter || c == '.')
        )
      // make sure it doesn't have a lwoer case or title case variant, common for titles and place names...
      if (hasAcronymShape  && !knownLowerCase && !hasTitleCaseVariant) {
        features += isProbablyAcronymFeature
      }

      // year!
      if (wlen == 4 && !hasNonDigit) {
        val year = try{w.toInt} catch {case e: NumberFormatException => 0}
        if (year >= 1400 && year < 2300) {
          features += isProbablyYearFeature
        }
      }

      if (hasDigit && !hasLetter) {
        try {
          val n = w.replaceAll(",","").toDouble
          if (!hasPeriod)
            features += integerFeature
          else
            features += floatFeature
        } catch {case e: NumberFormatException =>}
      }

      if (wlen > 3 && w.endsWith("s") && !w.endsWith("ss") && !w.endsWith("us") && !w.endsWith("is")) {
        features += endsWithSFeature
        if (hasInitialUpper)
          features += hasInitialCapsAndEndsWithSFeature // we mess up NNP and NNPS
      }

      if (wlen > 10) {
        features += longWordFeature
      } else if (wlen < 5) {
        features += shortWordFeature
      }
    }
    features
  }

  def apply(w: String) = featuresFor(w)

}

object WordPropertyFeaturizer {

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
  val hasKnownTitleCaseFeature = IndicatorWSFeature('HasKnownTC)
  val hasInitCapFeature = IndicatorWSFeature('HasInitCap)
  val hasInitialCapsAndEndsWithSFeature = IndicatorWSFeature('HasInitCapAndEndsWithS)
  val hasCapFeature = IndicatorWSFeature('HasCap)
  val hasManyCapFeature = IndicatorWSFeature('HasManyCap)
  val isAllCapsFeature = IndicatorWSFeature('AllCaps)
  val isProbablyAcronymFeature = IndicatorWSFeature('ProbablyAcronym)
  val isProbablyYearFeature = IndicatorWSFeature('ProbablyYear)
  val startOfSentenceFeature = IndicatorWSFeature('StartOfSentence)
  val integerFeature = IndicatorWSFeature('Integer)
  val floatFeature = IndicatorWSFeature('Float)
  val isAnInitialFeature = IndicatorWSFeature('IsAnInitial)
  val endsWithPeriodFeature = IndicatorWSFeature('EndsWithPeriod)
}

@SerialVersionUID(1L)
case class DigitNormalizedFeature(w: String) extends Feature
