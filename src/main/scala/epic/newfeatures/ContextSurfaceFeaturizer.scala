package epic.newfeatures

import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer

case class OffsetFeature(offset: Int, feature: Feature) extends Feature
case class BigramFeature(offset: Int, prev: Feature, next: Feature) extends Feature

/**
 *
 * @author dlwh
 */
class ContextSurfaceFeaturizer[W](val base: SurfaceFeaturizer[W]) extends SurfaceFeaturizer[W] {
  def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
    val b = base.anchor(w)
    def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Feature] = {
      if(level == FeaturizationLevel.MinimalFeatures) {
        b.featuresForWord(pos, level)
      } else {
        val result = new ArrayBuffer[Feature]()
        result ++= b.featuresForWord(pos, level)
        result ++= b.featuresForWord(pos-1, level.less).map(f => OffsetFeature(-1, f):Feature)
        result ++= b.featuresForWord(pos+1, level.less).map(f => OffsetFeature(+1, f):Feature)
        for(off <- -1 to 0;
            f1 <- b.featuresForWord(pos + off, FeaturizationLevel.MinimalFeatures);
            f2 <- b.featuresForWord(pos + off -1, FeaturizationLevel.MinimalFeatures)) {
          result +=  BigramFeature(off, f1, f2)
        }
        result.toArray
      }
    }

    def words: IndexedSeq[W] = w

    def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel): Array[Feature] = {
      b.featuresForSpan(begin, end, level)

    }
  }

}
