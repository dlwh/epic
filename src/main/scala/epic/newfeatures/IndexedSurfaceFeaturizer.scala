package epic
package newfeatures

import breeze.util.Index
import epic.framework.Feature
import epic.newfeatures.FeaturizationLevel.FullFeatures

/**
 *
 * @author dlwh
 */
trait IndexedSurfaceFeaturizer[Datum, W] extends IndexedWordFeaturizer[Datum, W] {
  def spanFeatureIndex: Index[Feature]
  def featurizer: SurfaceFeaturizer[W]
  def anchor(datum: Datum):IndexedSurfaceAnchoring[W]
}

trait IndexedSurfaceAnchoring[W] extends IndexedWordAnchoring[W] {
  def innerAnchoring: SurfaceFeatureAnchoring[W]
  def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel = FullFeatures):Array[Int]
}