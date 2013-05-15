package epic
package features

import breeze.collection.mutable.TriangularArray
import breeze.util.Index
import com.google.common.collect.MapMaker
import epic.framework.Feature
import epic.features.FeaturizationLevel.FullFeatures
import epic.util.Has2
import java.io.IOException
import java.util.Collections
import scala.collection.mutable

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
                         cache: Boolean = true)
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

    val ww = wordIndex
    val ss = spanIndex

    val f = new MySurfaceFeaturizer[Datum, W](feat, ww, ss)
    if(cache) new CachedFeaturizer(f)
    else f
  }

  @SerialVersionUID(1L)
  class CachedFeaturizer[Datum, W](val base: IndexedSurfaceFeaturizer[Datum, W]) extends IndexedSurfaceFeaturizer[Datum, W] with Serializable {
    def spanFeatureIndex: Index[Feature] = base.spanFeatureIndex

    def featurizer: SurfaceFeaturizer[W] = base.featurizer

    def wordFeatureIndex: Index[Feature] = base.wordFeatureIndex


    @transient
    private var cache = Collections.synchronizedMap(new MapMaker().softValues.makeMap[Datum, IndexedSurfaceAnchoring[W]]())
    def anchor(words: Datum): IndexedSurfaceAnchoring[W] = {
      val cached = cache.get(words)
      if(cached ne null) {
        cached
      } else {
        val x = base.anchor(words)
        cache.put(words, x)
        x
      }
    }

    @throws[IOException]
    @throws[ClassNotFoundException]
    private def readObject(stream: java.io.ObjectInputStream) {
      stream.defaultReadObject()
      cache = Collections.synchronizedMap(new MapMaker().softValues().makeMap[Datum, IndexedSurfaceAnchoring[W]]())
    }
  }

  @SerialVersionUID(1L)
  private class MySurfaceFeaturizer[Datum, W](val featurizer: SurfaceFeaturizer[W],
                                              val wordFeatureIndex: Index[Feature],
                                              val spanFeatureIndex: Index[Feature])
                                             (implicit hasWords: Has2[Datum, IndexedSeq[W]],
                                              hasConstraints: HasSpanConstraints[Datum]) extends IndexedSurfaceFeaturizer[Datum, W] with Serializable {
    def anchor(d: Datum):IndexedSurfaceAnchoring[W]  = {
      val words = hasWords.get(d)
      val cons = hasConstraints.get(d)
      val anch = featurizer.anchor(words)
      val wordFeatures = Array.tabulate(words.length, FeaturizationLevel.numLevels) { (i,l) =>
        stripEncode(wordFeatureIndex, anch.featuresForWord(i, l))
      }
      val spanFeatures = TriangularArray.tabulate(words.length+1){ (i, j) =>
        if(cons(i,j) && i < j) {
         Array.tabulate(FeaturizationLevel.numLevels){l => stripEncode(spanFeatureIndex, anch.featuresForSpan(i, j, l))}
        } else {
          null
        }
      }

      new TabulatedIndexedSurfaceAnchoring[W](words, wordFeatures, spanFeatures)

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