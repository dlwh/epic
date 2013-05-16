package epic.features

import epic.util.Has2
import breeze.util.Index
import epic.framework.Feature
import scala.collection.mutable

trait IndexedWordFeaturizer[Datum, W] {
  def wordFeatureIndex: Index[Feature]
  def featurizer: WordFeaturizer[W]
  def anchor(datum: Datum):IndexedWordAnchoring[W]
}

/**
 *
 * @author dlwh
 */
object IndexedWordFeaturizer {
  def fromData[Datum, W](feat: WordFeaturizer[W],
                         data: IndexedSeq[Datum],
                         wordHashFeatures: Int = 0)
                        (implicit hasWords: Has2[Datum, IndexedSeq[W]]): IndexedWordFeaturizer[Datum, W]  = {
    val wordIndex = Index[Feature]()
    for(d <- data) {
      val words = hasWords.get(d)
      val anch = feat.anchor(words)
      for(i <- 0 until words.length) {
        anch.featuresForWord(i) foreach {wordIndex.index _}
      }
    }


    new MyWordFeaturizer[Datum, W](feat, wordIndex)
  }

  @SerialVersionUID(1L)
  private class MyWordFeaturizer[Datum, W](val featurizer: WordFeaturizer[W],
                                           val wordFeatureIndex: Index[Feature])
                                          (implicit hasWords: Has2[Datum, IndexedSeq[W]]) extends IndexedWordFeaturizer[Datum, W] with Serializable {
    def anchor(d: Datum):IndexedWordAnchoring[W]  = {
      val words = hasWords.get(d)
      val anch = featurizer.anchor(words)
      val wordFeatures = Array.tabulate(words.length, FeaturizationLevel.numLevels) { (i,l) => stripEncode(wordFeatureIndex, anch.featuresForWord(i, l))}

      new TabulatedIndexedSurfaceAnchoring[W](words, wordFeatures, null)

    }
  }

  def stripEncode(ind: Index[Feature], features: Array[Feature]) = {
    val result = mutable.ArrayBuilder.make[Int]()
    result.sizeHint(features)
    var i = 0
    while(i < features.length) {
      val fi = ind(features(i))
      if(fi >= 0)
        result += fi
      i += 1
    }
    result.result()
  }
}


