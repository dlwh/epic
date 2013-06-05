package epic.features

import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer
import epic.util.Arrays

case class OffsetFeature(offset: Int, feature: Feature) extends Feature
case class SpanOffsetFeature(offset: Int, feature: Any) extends Feature
case class BigramFeature(offset: Int, prev: Feature, next: Feature) extends Feature

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class ContextSurfaceFeaturizer[W](val base: SurfaceFeaturizer[W], wordOffsetOrder:Int =1, spanOffsetOrder: Int = 1) extends SurfaceFeaturizer[W] with Serializable {
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

        val myFeats = b.featuresForWord(pos, FeaturizationLevel.MinimalFeatures)
        result ++= Arrays.crossProduct(Array(myFeats.head), b.featuresForWord(pos+1, FeaturizationLevel.MinimalFeatures)){BigramFeature(0, _, _)}
        result ++= Arrays.crossProduct(b.featuresForWord(pos-1, FeaturizationLevel.MinimalFeatures), Array(myFeats.head)){BigramFeature(-1, _, _)}
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
