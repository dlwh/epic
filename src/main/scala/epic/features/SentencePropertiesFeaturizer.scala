package epic.features

import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer

/**
 *
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SentencePropertiesFeaturizer(db: DistanceBinner = new DistanceBinner()) extends WordFeaturizer[String] with SurfaceFeaturizer[String] with Serializable {
  def anchor(words: IndexedSeq[String]): SurfaceFeatureAnchoring[String]  with WordFeatureAnchoring[String] = {
    val wholeSentenceIsUpperCase = words.forall(_.forall(c => !c.isLetter || c.isUpper))
    val sentenceLengthFeature = SentenceLengthFeature(db.binnedDistance(0,words.length))
    val w = words
    new SurfaceFeatureAnchoring[String] with WordFeatureAnchoring[String] {

      def words: IndexedSeq[String] = w

      def featuresForWord(pos: Int): Array[Feature] = featuresForSpan(pos, pos+1)

      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        val feats = new ArrayBuffer[Feature]()
        feats += sentenceLengthFeature
        if (wholeSentenceIsUpperCase)
          feats += WholeSentenceIsUpperCaseFeature
        // if (begin == 0)
        //  feats += BeginSentFeature
        // if (end == words.length)
        //  feats += EndSentFeature
        if (begin == 0 && end == words.length)
          feats += WholeSentFeature
        feats.toArray
      }
    }
  }
}

case object BeginSentFeature extends Feature
case object EndSentFeature extends Feature
case object WholeSentFeature extends Feature
