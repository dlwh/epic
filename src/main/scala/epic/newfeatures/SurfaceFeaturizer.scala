package epic.newfeatures

import epic.framework.Feature
import epic.newfeatures.SurfaceFeaturizer.{FullFeatures, FeaturizationLevel}

/**
 *
 * @author dlwh
 */
trait SurfaceFeaturizer[W] {
  def anchor(words: IndexedSeq[W]):Anchoring


  trait Anchoring {
    def words: IndexedSeq[W]
    def featuresForWord(pos: Int, level: FeaturizationLevel = FullFeatures):Array[Feature]
    def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel = FullFeatures):Array[Feature]
  }

}

object SurfaceFeaturizer {
  sealed abstract class FeaturizationLevel(val level: Int) {
    def less = FeaturizationLevel(level-1)
    def more = FeaturizationLevel(level+1)
  }
  case object MinimalFeatures extends FeaturizationLevel(0)
  case object BasicFeatures extends FeaturizationLevel(1)
  case object FullFeatures extends FeaturizationLevel(2)
  object FeaturizationLevel {
    def apply(level: Int) = level match {
      case x if x < 0 => MinimalFeatures
      case 0 => MinimalFeatures
      case 1 => BasicFeatures
      case 2 => FullFeatures
      case _ => FullFeatures
    }
  }
}
