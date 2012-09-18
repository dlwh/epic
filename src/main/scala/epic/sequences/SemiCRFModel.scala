package epic.sequences

import epic.framework._
import breeze.util.Index
import breeze.linalg.{SparseVector, DenseVector}
import epic.sequences.SemiCRF.{AnchoredFeaturizer, TransitionVisitor}
import collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */
class SemiCRFModel[L, W](val featureIndex: Index[Feature],
                         featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                         maxSegmentLength: Int=>Int) extends Model[Segmentation[L, W]] with StandardExpectedCounts.Model {
  type Inference = SemiCRFInference[L, W]

  def initialValueForFeature(f: Feature): Double = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new SemiCRFInference(weights, featureIndex, featurizer, maxSegmentLength)

}

object SemiCRFModel {
  trait BIEOFeaturizer[L, W] extends SemiCRF.IndexedFeaturizer[L, W] {
    def anchor(w: W): BIEOAnchoredFeaturizer[L, W]
  }

  trait BIEOAnchoredFeaturizer[L, W] extends SemiCRF.AnchoredFeaturizer[L, W] {

    def featuresForBegin(prev: Int, cur: Int, pos: Int):SparseVector[Double]
    def featuresForEnd(cur: Int, pos: Int):SparseVector[Double]
    def featuresForInterior(cur: Int, pos: Int):SparseVector[Double]
    def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int):SparseVector[Double]

    def featuresForTransition(prev: Int, cur: Int, start: Int, end: Int): SparseVector[Double] = {
      val acc = new ArrayBuffer[SparseVector[Double]]()
      var nnz =  0
      val _begin = featuresForBegin(prev, cur, start)
      acc += _begin
      nnz += _begin.activeSize
      val _end = featuresForEnd(cur, end)
      acc += _end
      nnz += _end.activeSize
      var p = start+1
      while(p < end) {
        val w = featuresForInterior(cur, p)
        acc += w
        nnz += w.activeSize
        p += 1
      }

      val forSpan = featuresForSpan(prev, cur, start, end)
      acc += forSpan
      nnz += forSpan.activeSize

      val result = SparseVector.zeros[Double](featureIndex.size)
      var i = 0
      while(i < acc.size) {
        result += acc(i)
        i += 1
      }
      result
    }
  }
}

class SemiCRFInference[L, W](weights: DenseVector[Double],
                             featureIndex: Index[Feature],
                             featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                             maxLength: Int=>Int) extends MarginalInference[Segmentation[L, W], SemiCRF.Anchoring[L, W]] {
  type Marginal = SemiCRF.Marginal[L, W]
  type ExpectedCounts = StandardExpectedCounts


  def marginal(v: Segmentation[L, W], aug: SemiCRF.Anchoring[L, W]): (Marginal, Double) = {
    val m = SemiCRF.Marginal(new Anchoring(v.words, v.length, aug))
    m -> m.logPartition
  }


  def goldCounts(v: Segmentation[L, W], augment: SemiCRF.Anchoring[L, W]): ExpectedCounts = {
    val m = SemiCRF.Marginal.goldMarginal(new Anchoring(v.words, v.length, augment), v.segments, v.words)
    this.countsFromMarginal(v, m, augment)
  }


  def countsFromMarginal(v: Segmentation[L, W], marg: Marginal, aug: SemiCRF.Anchoring[L, W]): ExpectedCounts = {
    val counts = StandardExpectedCounts.zero(featureIndex)
    counts.loss = marg.logPartition
    val localization = marg.anchoring.asInstanceOf[Anchoring].localization
    val visitor = new TransitionVisitor[L, W] {

      def daxpy(d: Double, vector: SparseVector[Double], counts: DenseVector[Double]) {
        var i = 0
        val index = vector.index
        val data = vector.data
        while(i < vector.iterableSize) {
//          if(vector.isActive(i))
            counts(index(i)) += d * data(i)
          i += 1
        }

      }

      def apply(prev: Int, cur: Int, start: Int, end: Int, count: Double) {
        import localization._
        daxpy(count, featuresForBegin(prev, cur, start), counts.counts)
        daxpy(count, featuresForEnd(cur, end), counts.counts)
        var p = start+1
        while(p < end) {
          daxpy(count, featuresForInterior(cur, p), counts.counts)
          p += 1
        }

        daxpy(count, featuresForSpan(prev, cur, start, end), counts.counts)

      }
    }
    marg.visit(visitor)
    counts

  }


  def baseAugment(v: Segmentation[L, W]): SemiCRF.Anchoring[L, W] = new SemiCRF.Anchoring[L, W] {
    def w: W = v.words

    def length: Int = v.length

    def maxSegmentLength(l: Int): Int = maxLength(l)

    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = 0.0

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol: L = featurizer.startSymbol
  }

  class Anchoring(val w: W, val length: Int, augment: SemiCRF.Anchoring[L, W]) extends SemiCRF.Anchoring[L, W] {
    val localization = featurizer.anchor(w)
    def maxSegmentLength(l: Int): Int = SemiCRFInference.this.maxLength(l)

    val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, length){ (p,c,w) =>
      weights dot localization.featuresForBegin(p,c,w)
    }
    val endCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      weights dot localization.featuresForEnd(l, w+1)
    }
    val wordCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      weights dot localization.featuresForInterior(l, w)
    }


    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      var score = augment.scoreTransition(prev, cur, beg, end)
      score += beginCache(prev)(cur)(beg)
      score += endCache(cur)(end-1)
      var pos = beg + 1
      while(pos < end) {
        score += wordCache(cur)(pos)
        pos += 1
      }

      score + (weights dot localization.featuresForSpan(prev, cur, beg, end))
    }

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol = featurizer.startSymbol
  }


}

