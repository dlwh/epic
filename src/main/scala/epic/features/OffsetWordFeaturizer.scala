package epic.features

import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(1L)
case class OffsetFeature(offset: Int, feature: Feature) extends Feature

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class OffsetWordFeaturizer[W](offsetFeaturizer: WordFeaturizer[W], offset:Int) extends WordFeaturizer[W] with Serializable {
  def anchor(w: IndexedSeq[W]): WordFeatureAnchoring[W] = new WordFeatureAnchoring[W] {
    val offsetAnchoring = offsetFeaturizer.anchor(w)
    def featuresForWord(pos: Int): Array[Feature] = {
      offsetAnchoring.featuresForWord(pos + offset).map(OffsetFeature(offset, _))
    }
    def words: IndexedSeq[W] = w
  }
}
