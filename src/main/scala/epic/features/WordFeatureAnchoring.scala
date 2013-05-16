package epic.features

import epic.framework.Feature
import breeze.util.Index
import epic.features.FeaturizationLevel.FullFeatures

/**
 *
 * @author dlwh
 */
trait WordFeatureAnchoring[W] {
  def words: IndexedSeq[W]
  def featuresForWord(pos: Int, level: FeaturizationLevel = FullFeatures):Array[Feature]
}
