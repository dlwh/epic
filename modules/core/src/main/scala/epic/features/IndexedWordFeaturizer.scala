package epic.features

import breeze.util.Index
import epic.framework.Feature

import scala.collection.mutable

trait IndexedWordFeaturizer[W] {
  def featureIndex: Index[Feature]
  def featurizer: WordFeaturizer[W]
  def anchor(datum: IndexedSeq[W]):IndexedWordAnchoring[W]
}

/**
 *
 * @author dlwh
 */
object IndexedWordFeaturizer {
  def fromData[W](feat: WordFeaturizer[W],
                  data: IndexedSeq[IndexedSeq[W]],
                  wordHashFeatures: Int = 0,
                  deduplicateFeatures: Boolean = true): IndexedWordFeaturizer[W]  = {
    val wordIndex = if (deduplicateFeatures) new NonRedundantIndexBuilder[Feature] else new NormalIndexBuilder[Feature]()
    for(words <- data) {
      val anch = feat.anchor(words)
      words.indices.foreach { i =>
        wordIndex.add(anch.featuresForWord(i) )
      }
    }

    new MyWordFeaturizer[W](feat, wordIndex.result())
  }

  @SerialVersionUID(1L)
  private class MyWordFeaturizer[W](val featurizer: WordFeaturizer[W],
                                    val featureIndex: Index[Feature]) extends IndexedWordFeaturizer[W] with Serializable {
    def anchor(words: IndexedSeq[W]):IndexedWordAnchoring[W]  = {
      val anch = featurizer.anchor(words)
      val wordFeatures = Array.tabulate(words.length) { i => stripEncode(featureIndex, anch.featuresForWord(i))}

      new TabulatedIndexedWordAnchoring[W](words, wordFeatures)
    }
  }

  private def stripEncode(ind: Index[Feature], features: Array[Feature]) = {
    val result = mutable.ArrayBuilder.make[Int]()
    result.sizeHint(features)
    var i = 0
    while (i < features.length) {
      val fi = ind(features(i))
      if (fi >= 0)
        result += fi
      i += 1
    }
    val r = result.result()
    assert(!r.isEmpty, features.toIndexedSeq + " " + ind)
    r
  }
}

@SerialVersionUID(1L)
class TabulatedIndexedWordAnchoring[W](val words: IndexedSeq[W],
                                       spanFeatures: Array[Array[Int]]) extends IndexedWordAnchoring[W] with Serializable {
  def featuresForWord(begin: Int):Array[Int] = {
    spanFeatures(begin)
  }

}
