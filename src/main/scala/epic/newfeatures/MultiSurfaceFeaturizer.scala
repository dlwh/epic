package epic.newfeatures

import epic.framework.Feature

/**
 *
 * @author dlwh
 */
class MultiSurfaceFeaturizer[W](feats: IndexedSeq[SurfaceFeaturizer[W]]) extends SurfaceFeaturizer[W] {
  def this(feats: SurfaceFeaturizer[W]*) = this(feats.toArray)

  def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
    val anchs = feats.map(_.anchor(w)).toArray
    def words: IndexedSeq[W] = w

    def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Feature] = anchs.flatMap(_.featuresForWord(pos, level))
    def featuresForSpan(beg: Int, end: Int, level: FeaturizationLevel): Array[Feature] = anchs.flatMap(_.featuresForSpan(beg, end, level))
  }
}
