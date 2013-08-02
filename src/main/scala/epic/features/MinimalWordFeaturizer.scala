package epic.features

import breeze.linalg.Counter
import epic.framework.Feature
import breeze.util.{Encoder, Interner, Index}
import epic.parser.features.IndicatorFeature
import chalk.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import scala.collection.immutable

/**
 * TODO: extract minimal features into their own class!
 *
 * @author dlwh
 **/
class MinimalWordFeaturizer(wordCounts: Counter[String, Double],
                             functionWordThreshold: Int = 100,
                             unknownWordThreshold: Int = 2) extends WordFeaturizer[String] with Serializable {

  def anchor(words: IndexedSeq[String]):WordFeatureAnchoring[String] = {
    val w = words
    new WordFeatureAnchoring[String] {
      val indices = words.map(wordIndex)
      val wholeSentenceIsUpperCase = words.forall(_.forall(c => !c.isLetter || c.isUpper))
      def words = w

      def featuresForWord(pos: Int): Array[Feature] = {
        if(pos < 0 || pos >= words.length) {
          boundaryFeatures
        } else {
           _minimalFeatures(pos)
        }
      }

      private val _minimalFeatures: immutable.IndexedSeq[Array[Feature]] = (0 until words.length) map { i =>
        val index = indices(i)
        if(index >= 0) {
          MinimalWordFeaturizer.this.minimalFeatures(index)
        } else {
          val ww = words(i)
          val classe = interner(WordFeature(EnglishWordClassGenerator(ww), 'Class))
          Array(/*shape,*/ classe, Unk)
        }
      }
    }
  }

  // more positional shapes to add

  private val wordIndex = Index(wordCounts.keySet)
  private val interner = new Interner[Feature]

  private val Unk = WordFeature("#UNK#", 'LowCount)
  private val boundaryFeatures = Array[Feature](BoundaryFeature)

  private val wordFeatures = Encoder.fromIndex(wordIndex).tabulateArray(s => if(wordCounts(s) > unknownWordThreshold) interner(IndicatorFeature(s)) else Unk)
  private val classes = Encoder.fromIndex(wordIndex).tabulateArray(w => if(wordCounts(w) > functionWordThreshold) wordFeatures(wordIndex(w)) else interner(WordFeature(EnglishWordClassGenerator(w), 'Class)))

  // caches
  private val minimalFeatures = Array.tabulate(wordIndex.size){ i =>
    val wc = wordCounts(wordIndex.get(i))
    val w = wordFeatures(i)
    val classe =  classes(i)
    if(wc > functionWordThreshold) Array(w)
    else if (wc > unknownWordThreshold) Array(w, /*shape,*/ classe)
    else Array(/*shape,*/ classe, Unk)
  }

}
