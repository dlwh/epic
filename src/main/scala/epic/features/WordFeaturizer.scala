package epic.features

/**
 *
 * @author dlwh
 */
trait WordFeaturizer[W] {
  def anchor(words: IndexedSeq[W]):WordFeatureAnchoring[W]

  def +(other: WordFeaturizer[W]) = (this,other) match {
    case (MultiWordFeaturizer(feats),MultiWordFeaturizer(feats2)) => new MultiWordFeaturizer(feats ++ feats2)
    case (MultiWordFeaturizer(feats),_) => new MultiWordFeaturizer(feats :+ other)
    case (_,MultiWordFeaturizer(feats2)) => new MultiWordFeaturizer(this +: feats2)
    case _ => new MultiWordFeaturizer(this, other)
  }
  def *(other:WordFeaturizer[W]) = new ProductWordFeaturizer(this, other)
  def offset(i: Int) = new OffsetWordFeaturizer(this, i)
}
