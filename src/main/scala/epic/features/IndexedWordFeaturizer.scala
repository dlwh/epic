package epic.features

import epic.util.Has2
import breeze.util.Index
import epic.framework.Feature

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

    val ww = new FeatureIndex(wordIndex, wordHashFeatures)

    new MyWordFeaturizer[Datum, W](feat, ww)
  }

  @SerialVersionUID(1L)
  private class MyWordFeaturizer[Datum, W](val featurizer: WordFeaturizer[W],
                                           val wordFeatureIndex: FeatureIndex)
                                          (implicit hasWords: Has2[Datum, IndexedSeq[W]]) extends IndexedWordFeaturizer[Datum, W] with Serializable {
    def anchor(d: Datum):IndexedWordAnchoring[W]  = {
      val words = hasWords.get(d)
      val anch = featurizer.anchor(words)
      val wordFeatures = Array.tabulate(words.length, FeaturizationLevel.numLevels) { (i,l) => wordFeatureIndex.stripEncode(anch.featuresForWord(i, l))}

      new TabulatedIndexedSurfaceAnchoring[W](words, wordFeatures, null)

    }
  }
}


