package epic
package newfeatures

import breeze.util.Index
import epic.framework.Feature
import epic.newfeatures.FeaturizationLevel.FullFeatures
import epic.util.Has2
import breeze.collection.mutable.TriangularArray

/**
 *
 * @author dlwh
 */
trait IndexedSurfaceFeaturizer[Datum, W] extends IndexedWordFeaturizer[Datum, W] {
  def spanFeatureIndex: Index[Feature]
  def featurizer: SurfaceFeaturizer[W]
  def anchor(datum: Datum):IndexedSurfaceAnchoring[W]
}

trait IndexedSurfaceAnchoring[W] extends IndexedWordAnchoring[W] {
  def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel = FullFeatures):Array[Int]
}

object IndexedSurfaceFeaturizer {
  def fromData[Datum, W](feat: SurfaceFeaturizer[W],
                         data: IndexedSeq[Datum],
                         wordHashFeatures: Int = 0,
                         spanHashFeatures: Int = 0)
                        (implicit hasWords: Has2[Datum, IndexedSeq[W]],
                         hasConstraints: HasSpanConstraints[Datum]): IndexedSurfaceFeaturizer[Datum, W]  = {
    val spanIndex = Index[Feature]()
    val wordIndex = Index[Feature]()
    for(d <- data) {
      val words = hasWords.get(d)
      val cons = hasConstraints.get(d)
      val anch = feat.anchor(words)
      for(i <- 0 until words.length) {
        anch.featuresForWord(i) foreach {wordIndex.index _}
        for(j <- (i+1) to (i + cons.maxSpanLengthStartingAt(i)) if cons(i, j)) {
          anch.featuresForSpan(i,j) foreach {spanIndex.index _}
        }
      }
    }

    val ww = new FeatureIndex(wordIndex, wordHashFeatures)
    val ss = new FeatureIndex(spanIndex, spanHashFeatures)

    new MySurfaceFeaturizer[Datum, W](feat, ww, ss)
  }

  @SerialVersionUID(1L)
  private class MySurfaceFeaturizer[Datum, W](val featurizer: SurfaceFeaturizer[W],
                                              val wordFeatureIndex: FeatureIndex,
                                              val spanFeatureIndex: FeatureIndex)
                                             (implicit hasWords: Has2[Datum, IndexedSeq[W]],
                                              hasConstraints: HasSpanConstraints[Datum]) extends IndexedSurfaceFeaturizer[Datum, W] with Serializable {
    def anchor(d: Datum):IndexedSurfaceAnchoring[W]  = {
      val words = hasWords.get(d)
      val cons = hasConstraints.get(d)
      val anch = featurizer.anchor(words)
      val wordFeatures = Array.tabulate(words.length, FeaturizationLevel.numLevels) { (i,l) => wordFeatureIndex.stripEncode(anch.featuresForWord(i, l))}
      val spanFeatures = TriangularArray.tabulate(words.length+1){ (i, j) =>
        if(cons(i,j) && i < j) {
         Array.tabulate(FeaturizationLevel.numLevels){l => spanFeatureIndex.stripEncode(anch.featuresForSpan(i, j, l))}
        } else {
          null
        }
      }

      new TabulatedIndexedSurfaceAnchoring[W](words, wordFeatures, spanFeatures)

    }
  }
}

class TabulatedIndexedSurfaceAnchoring[W](val words: IndexedSeq[W],
                                          wordFeatures: Array[Array[Array[Int]]],
                                          spanFeatures: TriangularArray[Array[Array[Int]]]) extends IndexedSurfaceAnchoring[W] {
  def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel = FullFeatures):Array[Int] = {
    val f = spanFeatures(begin, end)
    if(f eq null) null
    else f(level.level)
  }

  def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Int] = wordFeatures(pos)(level.level)
}