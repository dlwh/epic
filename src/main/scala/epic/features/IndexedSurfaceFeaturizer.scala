package epic
package features

import breeze.collection.mutable.TriangularArray
import breeze.util.Index
import epic.constraints.SpanConstraints
import epic.framework.Feature

import scala.collection.mutable

/**
 *
 * @author dlwh
 */
trait IndexedSurfaceFeaturizer[W] {
  def featureIndex: Index[Feature]
  def featurizer: SurfaceFeaturizer[W]
  def anchor(datum: IndexedSeq[W]):IndexedSurfaceAnchoring[W]
}

trait IndexedSurfaceAnchoring[W] {
  def words: IndexedSeq[W]
  def featuresForSpan(begin: Int, end: Int):Array[Int]
}

object IndexedSurfaceFeaturizer {
  def fromData[W](feat: SurfaceFeaturizer[W],
                  data: IndexedSeq[IndexedSeq[W]],
                  constraintFactory: SpanConstraints.Factory[W],
                  deduplicateFeatures: Boolean = false) : IndexedSurfaceFeaturizer[W]  = {

    val index = if (deduplicateFeatures) new NonRedundantIndexBuilder[Feature] else new NormalIndexBuilder[Feature]()

    for(words <- data) {
      val cons = constraintFactory.get(words)
      val anch = feat.anchor(words)
      words.indices.foreach { i =>
        for(j <- (i+1) to math.min(words.length, i + cons.maxSpanLengthStartingAt(i)) if cons(i, j)) {
          index.add(anch.featuresForSpan(i, j) )
        }
      }
    }

    new MySurfaceFeaturizer[W](feat, constraintFactory, index.result())
  }

  @SerialVersionUID(1L)
  class CachedFeaturizer[W](val base: IndexedSurfaceFeaturizer[W], cache: collection.mutable.Map[IndexedSeq[W], IndexedSurfaceAnchoring[W]]) extends IndexedSurfaceFeaturizer[W] with Serializable {
    def featurizer: SurfaceFeaturizer[W] = base.featurizer
    def featureIndex: Index[Feature] = base.featureIndex
    def anchor(datum: IndexedSeq[W]): IndexedSurfaceAnchoring[W] = cache.getOrElseUpdate(datum, base.anchor(datum))
  }

  @SerialVersionUID(3L)
  private class MySurfaceFeaturizer[W](val featurizer: SurfaceFeaturizer[W],
                                       constraintsFactory: SpanConstraints.Factory[W],
                                       val featureIndex: Index[Feature]) extends IndexedSurfaceFeaturizer[W] with Serializable {
    def anchor(words: IndexedSeq[W]):IndexedSurfaceAnchoring[W]  = {
      val cons = constraintsFactory.constraints(words)
      val anch = featurizer.anchor(words)
      val spanFeatures = TriangularArray.tabulate(words.length+1){ (i, j) =>
        if (cons(i,j) && i < j) {
          stripEncode(featureIndex, anch.featuresForSpan(i, j))
        } else {
          null
        }
      }

      new TabulatedIndexedSurfaceAnchoring[W](words, spanFeatures)

    }
  }

  def stripEncode(ind: Index[Feature], features: Array[Feature]) = {
    val result = mutable.ArrayBuilder.make[Int]()
    result.sizeHint(features)
    var i = 0
    while (i < features.length) {
      val fi = ind(features(i))
      if (fi >= 0)
        result += fi
      i += 1
    }
    result.result()
  }
}

@SerialVersionUID(2L)
class TabulatedIndexedSurfaceAnchoring[W](val words: IndexedSeq[W],
                                          spanFeatures: TriangularArray[Array[Int]]) extends IndexedSurfaceAnchoring[W] with Serializable {
  def featuresForSpan(begin: Int, end: Int):Array[Int] = {
    spanFeatures(begin, end)
  }

}
