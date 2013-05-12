package epic.newfeatures

import epic.framework.Feature
import breeze.util.Index
import epic.newfeatures.FeaturizationLevel.FullFeatures

/**
  *
  * @author dlwh
  */
trait SurfaceFeatureAnchoring[W] extends WordFeatureAnchoring[W] {
  def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel = FullFeatures):Array[Feature]
}
