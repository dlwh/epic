package epic.features

/*
 * @author dlwh
 */
trait SurfaceFeaturizer[W] extends WordFeaturizer[W] {
  def anchor(words: IndexedSeq[W]):SurfaceFeatureAnchoring[W]
}

sealed abstract class FeaturizationLevel(val level: Int) {
  def less = FeaturizationLevel(level-1)
  def more = FeaturizationLevel(level+1)
}
object FeaturizationLevel {
  case object MinimalFeatures extends FeaturizationLevel(0)
  case object BasicFeatures extends FeaturizationLevel(1)
  case object FullFeatures extends FeaturizationLevel(2)
  val numLevels = 3
  implicit def apply(level: Int) = level match {
    case x if x < 0 => MinimalFeatures
    case 0 => MinimalFeatures
    case 1 => BasicFeatures
    case 2 => FullFeatures
    case _ => FullFeatures
  }
}
