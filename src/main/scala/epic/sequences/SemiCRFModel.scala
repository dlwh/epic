package epic.sequences

import epic.framework._
import breeze.util.Index
import breeze.linalg.{SparseVector, DenseVector}
import epic.sequences.SemiCRF.TransitionVisitor

/**
 *
 * @author dlwh
 */
class SemiCRFModel[L, W](val featureIndex: Index[Feature],
                         featurizer: SemiCRF.IndexedFeaturizer[L, W],
                         maxSegmentLength: Int=>Int) extends Model[Segmentation[L, W]] with StandardExpectedCounts.Model {
  type Inference = SemiCRFInference[L, W]

  def initialValueForFeature(f: Feature): Double = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new SemiCRFInference(weights, featureIndex, featurizer, maxSegmentLength)

}

class SemiCRFInference[L, W](weights: DenseVector[Double],
                             featureIndex: Index[Feature],
                             featurizer: SemiCRF.IndexedFeaturizer[L, W],
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
        while(i < vector.activeSize) {
          counts(vector.indexAt(i)) += d * vector.valueAt(i)
          i += 1
        }

      }

      def apply(prev: Int, cur: Int, beg: Int, end: Int, count: Double) {
        daxpy(count, localization.featuresForTransition(prev, cur, beg, end), counts.counts)

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

    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      (weights dot localization.featuresForTransition(prev, cur, beg, end)) + augment.scoreTransition(prev, cur, beg, end)
    }

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol = featurizer.startSymbol
  }


}

