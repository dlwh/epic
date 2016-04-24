package epic.features

import epic.framework.Feature

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
case class MultiSurfaceFeaturizer[W](feats: IndexedSeq[SurfaceFeaturizer[W]]) extends SurfaceFeaturizer[W] with Serializable {
  def this(feats: SurfaceFeaturizer[W]*) = this(feats.toArray)

  def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
    val anchs = feats.map(_.anchor(w)).toArray
    def words: IndexedSeq[W] = w
    def featuresForSpan(beg: Int, end: Int): Array[Feature] = anchs.flatMap(_.featuresForSpan(beg, end))
  }
}
