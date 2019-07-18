package epic.features

import breeze.linalg.Counter
import epic.framework.Feature
import breeze.util.{Encoder, Interner, Index}
import scala.collection.immutable

/**
 *
 *
 * @author dlwh
 **/
@SerialVersionUID(1L)
class IdentityWordFeaturizer[W](wordCounts: Counter[W, Double], unknownWordThreshold: Int = 2) extends WordFeaturizer[W] with Serializable {

  def anchor(words: IndexedSeq[W]):WordFeatureAnchoring[W] = {
    val w = words
    new WordFeatureAnchoring[W] {
      val indices = words.map(wordIndex)
      def words = w

      def featuresForWord(pos: Int): Array[Feature] = {
        if (pos < 0 || pos >= words.length) {
          boundaryFeatures
        } else {
           _minimalFeatures(pos)
        }
      }

      private val _minimalFeatures: immutable.IndexedSeq[Array[Feature]] = words.indices.map { i =>
        val index = indices(i)
        if (index >= 0) {
          IdentityWordFeaturizer.this.minimalFeatures(index)
        } else {
          Array[Feature](Unk)
        }
      }
    }
  }

  // more positional shapes to add

  private val wordIndex = Index(wordCounts.keySet)
  private val Unk = WordFeature("#UNK#", 'LowCount)
  private val boundaryFeatures = Array[Feature](BoundaryFeature)
  private val wordFeatures = Encoder.fromIndex(wordIndex).tabulateArray(s => if (wordCounts(s) > unknownWordThreshold) IndicatorFeature(s) else Unk)

  // caches
  private val minimalFeatures = Array.tabulate[Array[Feature]](wordIndex.size){ i =>
    val wc = wordCounts(wordIndex.get(i))
    val w = wordFeatures(i)
    if (wc > unknownWordThreshold) {
      Array(w)
    } else {
      Array(Unk)
    }
  }

}
