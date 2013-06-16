package epic.features

import epic.framework.Feature
import breeze.linalg.Counter
import breeze.util.{Encoder, Interner, Index}
import scala.Array
import epic.parser.features.{PairFeature, IndicatorFeature}
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import scala.collection.mutable.ArrayBuffer
import epic.parser.features.StandardSpanFeatures._
import epic.features.FeaturizationLevel.{FullFeatures, BasicFeatures, MinimalFeatures}

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class StandardSurfaceFeaturizer(wordFeaturizer: WordFeaturizer[String]) extends SurfaceFeaturizer[String] with Serializable {
  def this(wordCounts: Counter[String, Double],
           functionWordThreshold: Int = 100,
           commonWordThreshold: Int = 20,
           unknownWordThreshold: Int = 2)  = this(new StandardWordFeaturizer(wordCounts, functionWordThreshold, commonWordThreshold, unknownWordThreshold))

  def anchor(words: IndexedSeq[String]):SurfaceFeatureAnchoring[String] = {
    val w = words
    val wordAnch = wordFeaturizer.anchor(w)
    new SurfaceFeatureAnchoring[String] {
      val wholeSentenceIsUpperCase = words.forall(_.forall(c => !c.isLetter || c.isUpper))
      def words = w
      val sentenceLengthFeature = SentenceLengthFeature(distanceBinner.binnedDistance(0,words.length))


      def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Feature] = wordAnch.featuresForWord(pos, level)

      def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel): Array[Feature] = {
        val feats = ArrayBuffer[Feature]()
        if (begin < end - 1) {
          for(f1 <- featuresForWord(begin, level.less); f2 <- featuresForWord(end, level.less)) {
            feats += WordEdges('BeginEnd, f1, f2)
          }
          if(level.level >= FeaturizationLevel.BasicFeatures.level) {
            for(f1 <- featuresForWord(begin, level.less); f2 <- featuresForWord(end-1, level.less)) {
              feats += WordEdges('Inside, f1, f2)
            }

            for(f1 <- featuresForWord(begin-1, level.less); f2 <- featuresForWord(end, level.less)) {
              feats += WordEdges('Outside, f1, f2)
            }


            for(f1 <- featuresForWord(begin, level.less)) {
              feats += WordBoundary('Begin, f1)
            }

            for(f1 <- featuresForWord(end-1, level.less)) {
              feats += WordBoundary('Outside, f1)
            }
          }


        }

        if(level.level >= FeaturizationLevel.BasicFeatures.level) {
          feats += sentenceLengthFeature
          if(wholeSentenceIsUpperCase)
            feats += WholeSentenceIsUpperCaseFeature
        }


        if (begin == 0)
          feats += BeginSentFeature
        if(end == words.length)
          feats += EndSentFeature
        if (begin == 0 && end == words.length)
          feats += WholeSentFeature

        if(level.level >= BasicFeatures.level) {
          feats += SpanShapeFeature(SpanShapeGenerator.signatureFor(words, begin, end, includeContext = true))
          feats += SpanShapeFeature(SpanShapeGenerator.signatureFor(words, begin, end, includeContext = false))
        }

        if(level == FullFeatures) {
          val spanLength = SpanLengthFeature(distanceBinner.binnedDistance(begin, end))
          for(f <- feats.toArray) {
            feats += PairFeature(f, spanLength)
          }
          feats += spanLength
        } else {
          feats += SpanLengthFeature(distanceBinner.binnedDistance(begin, end))
        }

        feats.toArray
      }


    }

  }

  private val distanceBinner = DistanceBinner()

}

case class FirstWordCapsAnd(f: Feature) extends Feature
case class NthWordCapsAnd(f: Feature) extends Feature
case class SentenceLengthFeature(length: Int) extends Feature
case object WholeSentenceIsUpperCaseFeature extends Feature
case class WordFeature(word: Any, kind: Symbol) extends Feature

case object BoundaryFeature extends Feature
