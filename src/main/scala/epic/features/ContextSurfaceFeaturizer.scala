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
class ContextSurfaceFeaturizer[W](val base: WordFeaturizer[W], spanOffsetOrder: Int = 1) extends SurfaceFeaturizer[W] with Serializable {
  def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
    val b = base.anchor(w)

    def words: IndexedSeq[W] = w

    def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
      val result = new ArrayBuffer[Feature]()
      for(off <- -spanOffsetOrder to spanOffsetOrder if off != 0) {
        if(off < 0) { // eh
          result ++= b.featuresForWord(begin + off).map(f => SpanOffsetFeature(off, f):Feature)
        } else {
          result ++= b.featuresForWord(end - 1 + off).map(f => SpanOffsetFeature(off, f):Feature)
        }
      }
      result.toArray
    }
  }

}
