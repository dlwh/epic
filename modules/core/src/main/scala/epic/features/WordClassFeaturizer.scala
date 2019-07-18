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
class WordClassFeaturizer(wordCounts: Counter[String, Double],
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

      private val _minimalFeatures: immutable.IndexedSeq[Array[Feature]] = words.indices.map { i =>
        val index = indices(i)
        if (index >= 0) {
          WordClassFeaturizer.this.minimalFeatures(index)
        } else {
          val ww = words(i)
          val classe = interner(WordFeature(EnglishWordClassGenerator(ww), 'Class))
          Array[Feature](classe)
        }
      }
    }
  }

  // more positional classes to add

  private val wordIndex = Index(wordCounts.keySet)
  private val interner = new Interner[Feature]

  private val boundaryFeatures = Array[Feature](BoundaryFeature)

  private val classes =  Encoder.fromIndex(wordIndex).tabulateArray(w => if (wordCounts(w) > functionWordThreshold) interner(IndicatorFeature(w)) else interner(WordFeature(EnglishWordClassGenerator(w), 'Class)))

  // caches
  private val minimalFeatures = Array.tabulate(wordIndex.size){ i =>
    Array(classes(i))
  }

}
