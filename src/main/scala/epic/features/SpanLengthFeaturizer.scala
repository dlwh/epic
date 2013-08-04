package epic.features

import epic.framework.Feature
import epic.parser.features.StandardSpanFeatures.{WholeSentFeature, EndSentFeature, BeginSentFeature, SpanLengthFeature}
import scala.collection.mutable.ArrayBuffer

/**
 *
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SpanLengthFeaturizer(db: DistanceBinner = new DistanceBinner()) extends SurfaceFeaturizer[String] with Serializable {
  def anchor(words: IndexedSeq[String]): SurfaceFeatureAnchoring[String] = {
    new SurfaceFeatureAnchoring[String] {
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        Array(SpanLengthFeature(db.binnedDistance(begin, end)))
      }
    }
  }
}
