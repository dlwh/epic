package epic.features

/**
 *
 * @author dlwh
 */
trait WordFeaturizer[W] {
  def anchor(words: IndexedSeq[W]):WordFeatureAnchoring[W]

  def +(other: WordFeaturizer[W]) = this match {
    case MultiWordFeaturizer(feats) => new MultiWordFeaturizer(this +: feats)
    case _ => new MultiWordFeaturizer(this, other)
  }
  def *(other:WordFeaturizer[W]) = new ProductWordFeaturizer(this, other)
  def offset(i: Int) = new OffsetWordFeaturizer(this, i)
}
