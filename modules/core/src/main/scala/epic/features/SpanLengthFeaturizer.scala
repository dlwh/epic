package epic.features

import epic.framework.Feature

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
@SerialVersionUID(1L)
case class SpanLengthFeature(dist: Int) extends Feature
