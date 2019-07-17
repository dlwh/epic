package epic.features

import breeze.linalg.Counter
import epic.framework.Feature
import breeze.util.{Encoder, Interner, Index}

/**
 *
 *
 * @author dlwh
 **/
@SerialVersionUID(1L)
class WordShapeFeaturizer(wordCounts: Counter[String, Double],
                          functionWordThreshold: Int = 100) extends WordFeaturizer[String] with Serializable {

  def anchor(words: IndexedSeq[String]):WordFeatureAnchoring[String] = {
    val w = words
    new WordFeatureAnchoring[String] {
      val indices = words.map(wordIndex)
      def words = w

      def featuresForWord(pos: Int): Array[Feature] = {
        if (pos < 0 || pos >= words.length) {
          boundaryFeatures
        } else {
           _minimalFeatures(pos)
        }
      }

      private val _minimalFeatures: IndexedSeq[Array[Feature]] = words.indices.map { i =>
        val index = indices(i)
        if (index >= 0) {
          WordShapeFeaturizer.this.minimalFeatures(index)
        } else {
          val ww = words(i)
          val shape = interner(WordFeature(WordShapeGenerator(ww), 'Shape))
          Array[Feature](shape)
        }
      }
    }
  }

  // more positional shapes to add

  private val wordIndex = Index(wordCounts.keySet)
  private val interner = new Interner[Feature]

  private val boundaryFeatures = Array[Feature](BoundaryFeature)

  private val shapes =  Encoder.fromIndex(wordIndex).tabulateArray(w => if (wordCounts(w) > functionWordThreshold) interner(IndicatorFeature(w)) else interner(WordFeature(WordShapeGenerator(w), 'Shape)))

  // caches
  private val minimalFeatures = Array.tabulate(wordIndex.size){ i =>
    Array(shapes(i))
  }

}
