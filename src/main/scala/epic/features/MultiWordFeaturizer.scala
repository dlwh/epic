package epic.features

import epic.framework.Feature

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
case class MultiWordFeaturizer[W](featurizers: IndexedSeq[WordFeaturizer[W]]) extends WordFeaturizer[W] with Serializable {
  def this(feats: WordFeaturizer[W]*) = this(feats.toArray)

  def anchor(w: IndexedSeq[W]): WordFeatureAnchoring[W] = new WordFeatureAnchoring[W] {
    val anchs = featurizers.map(_.anchor(w)).toArray
    def words: IndexedSeq[W] = w
    def featuresForWord(pos: Int): Array[Feature] = anchs.flatMap(_.featuresForWord(pos))
  }
}
