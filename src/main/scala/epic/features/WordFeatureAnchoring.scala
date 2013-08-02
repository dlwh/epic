package epic.features

import epic.framework.Feature

/**
 *
 * @author dlwh
 */
trait WordFeatureAnchoring[W] {
  def words: IndexedSeq[W]
  def featuresForWord(pos: Int):Array[Feature]
}
