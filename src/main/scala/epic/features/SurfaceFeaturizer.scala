package epic.features

/**
 * TODO
 * @author dlwh
 */
trait SurfaceFeaturizer[W] {
  def anchor(words: IndexedSeq[W]):SurfaceFeatureAnchoring[W]
  def +(other: SurfaceFeaturizer[W]) = new MultiSurfaceFeaturizer(this, other)
}
