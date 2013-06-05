package epic.features

/**
 *
 * @author dlwh
 */
trait WordFeaturizer[W] {
  def anchor(words: IndexedSeq[W]):WordFeatureAnchoring[W]

  def +(other: WordFeaturizer[W]) = new MultiWordFeaturizer(this, other)
}
