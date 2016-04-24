package epic.features

import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer
import epic.util.Arrays

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class ContextWordFeaturizer[W](offsetFeaturizer: WordFeaturizer[W], wordOffsetOrder:Int =1) extends WordFeaturizer[W] with Serializable {
  def anchor(w: IndexedSeq[W]): WordFeatureAnchoring[W] = new WordFeatureAnchoring[W] {
    val offsetAnchoring = offsetFeaturizer.anchor(w)

    def featuresForWord(pos: Int): Array[Feature] = {
      val result = new ArrayBuffer[Feature]()
      for(off <- -wordOffsetOrder to wordOffsetOrder if off != 0) {
        result ++= offsetAnchoring.featuresForWord(pos + off).map(f => OffsetFeature(off, f):Feature)
      }
      /*
      val myFeats = offsetAnchoring.featuresForWord(pos)
      result ++= Arrays.crossProduct(Array(myFeats.head), offsetAnchoring.featuresForWord(pos+1)){BigramFeature(0, _, _)}
      result ++= Arrays.crossProduct(offsetAnchoring.featuresForWord(pos-1), Array(myFeats.head)){BigramFeature(-1, _, _)}
      */
      result.toArray
    }

    def words: IndexedSeq[W] = w

  }

}
