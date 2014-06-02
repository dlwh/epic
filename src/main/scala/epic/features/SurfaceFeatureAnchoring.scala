package epic.features

import epic.framework.Feature

/**
  *
  * @author dlwh
  */
trait SurfaceFeatureAnchoring[W]  {
  def featuresForSpan(begin: Int, end: Int):Array[Feature]
}
