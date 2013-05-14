package epic.newfeatures

import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer

case class OffsetFeature(offset: Int, feature: Feature) extends Feature
case class SpanOffsetFeature(offset: Int, feature: Any) extends Feature
case class BigramFeature(offset: Int, prev: Feature, next: Feature) extends Feature

/**
 *
 * @author dlwh
 */
class ContextSurfaceFeaturizer[W](val base: SurfaceFeaturizer[W], wordOffsetOrder:Int =1, spanOffsetOrder: Int = 1) extends SurfaceFeaturizer[W] {
  def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
    val b = base.anchor(w)
    def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Feature] = {
      if(level == FeaturizationLevel.MinimalFeatures) {
        b.featuresForWord(pos, level)
      } else {
        val result = new ArrayBuffer[Feature]()
        result ++= b.featuresForWord(pos, level)
        for(off <- -wordOffsetOrder to wordOffsetOrder if off != 0) {
          result ++= b.featuresForWord(pos + off, level.less).map(f => OffsetFeature(off, f):Feature)
        }
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
      if(level == FeaturizationLevel.MinimalFeatures || begin + 1 >= end) {
        b.featuresForSpan(begin, end, level)
      } else {
        val result = new ArrayBuffer[Feature]()
        result ++= b.featuresForSpan(begin, end, level)
        for(off <- -spanOffsetOrder to spanOffsetOrder if off != 0) {
          if(off < 0) { // eh
            result ++= b.featuresForWord(begin + off, level.less).map(f => SpanOffsetFeature(off, f):Feature)
          } else {
            result ++= b.featuresForWord(end - 1 + off, level.less).map(f => SpanOffsetFeature(off, f):Feature)
          }
        }
        result.toArray
      }
    }
  }

}
