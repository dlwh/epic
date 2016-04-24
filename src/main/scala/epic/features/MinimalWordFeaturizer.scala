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
class MinimalWordFeaturizer(wordCounts: Counter[String, Double], includeWordShapeFeatures: Boolean = false,
                             functionWordThreshold: Int = 100,
                             unknownWordThreshold: Int = 2) extends WordFeaturizer[String] with Serializable {

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
          MinimalWordFeaturizer.this.minimalFeatures(index)
        } else {
          val ww = words(i)
          val classe = interner(WordFeature(EnglishWordClassGenerator(ww), 'Class))
          val shape = interner(WordFeature(WordShapeGenerator(ww), 'Shape))
          if (includeWordShapeFeatures) {
            Array(shape, classe, Unk)
          } else{
            Array(classe, Unk)
          }
        }
      }
    }
  }

  // more positional shapes to add

  private val wordIndex = Index(wordCounts.keySet)
  private val interner = new Interner[Feature]

  private val Unk = WordFeature("#UNK#", 'LowCount)
  private val boundaryFeatures = Array[Feature](BoundaryFeature)

  private val wordFeatures = Encoder.fromIndex(wordIndex).tabulateArray(s => if (wordCounts(s) > unknownWordThreshold) interner(IndicatorFeature(s)) else Unk)
  private val classes = Encoder.fromIndex(wordIndex).tabulateArray(w => if (wordCounts(w) > functionWordThreshold) wordFeatures(wordIndex(w)) else interner(WordFeature(EnglishWordClassGenerator(w), 'Class)))
  private val shapes = if (includeWordShapeFeatures) Encoder.fromIndex(wordIndex).tabulateArray(w => if (wordCounts(w) > functionWordThreshold) wordFeatures(wordIndex(w)) else interner(WordFeature(WordShapeGenerator(w), 'Shape))) else null

  // caches
  private val minimalFeatures = Array.tabulate(wordIndex.size){ i =>
    val wc = wordCounts(wordIndex.get(i))
    val w = wordFeatures(i)
    val classe =  classes(i)
    if (wc > functionWordThreshold) Array(w)
    else if (includeWordShapeFeatures) {
      val shape = shapes(i)
      if (wc > unknownWordThreshold) Array(w, shape, classe)
      else Array(shape, classe, Unk)
    } else if (wc > unknownWordThreshold) {
      Array(w, classe)
    } else {
      Array(classe, Unk)
    }
  }

}
