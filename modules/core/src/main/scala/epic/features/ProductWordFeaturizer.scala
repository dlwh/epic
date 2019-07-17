package epic.features

import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer
import epic.util.Arrays


/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class ProductWordFeaturizer[W](f1: WordFeaturizer[W], f2: WordFeaturizer[W]) extends WordFeaturizer[W] with Serializable {
  def anchor(w: IndexedSeq[W]): WordFeatureAnchoring[W] = new WordFeatureAnchoring[W] {
    val f1Anchoring = f1.anchor(w)
    val f2Anchoring = f2.anchor(w)

    def featuresForWord(pos: Int): Array[Feature] = {
      Arrays.crossProduct(f1Anchoring.featuresForWord(pos), f2Anchoring.featuresForWord(pos))(CrossProductFeature(_, _))
    }

    def words: IndexedSeq[W] = w
  }

}
