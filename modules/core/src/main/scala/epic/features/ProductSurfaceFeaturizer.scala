package epic.features

import epic.framework.Feature
import epic.util.Arrays


/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class ProductSurfaceFeaturizer[W](f1: SurfaceFeaturizer[W], f2: SurfaceFeaturizer[W]) extends SurfaceFeaturizer[W] with Serializable {
  def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
    val f1Anchoring = f1.anchor(w)
    val f2Anchoring = f2.anchor(w)

    def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
      Arrays.crossProduct(f1Anchoring.featuresForSpan(begin, end), f2Anchoring.featuresForSpan(begin, end))(CrossProductFeature(_, _))
    }

    def words: IndexedSeq[W] = w
  }

}
