package epic.features

import epic.framework.Feature

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class MultiWordFeaturizer[W](feats: IndexedSeq[WordFeaturizer[W]]) extends WordFeaturizer[W] with Serializable {
  def this(feats: WordFeaturizer[W]*) = this(feats.toArray)

  def anchor(w: IndexedSeq[W]): WordFeatureAnchoring[W] = new WordFeatureAnchoring[W] {
    val anchs = feats.map(_.anchor(w)).toArray
    def words: IndexedSeq[W] = w

    def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Feature] = anchs.flatMap(_.featuresForWord(pos, level))
  }
}
