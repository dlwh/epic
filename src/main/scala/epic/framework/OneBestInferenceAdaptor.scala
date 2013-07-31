package epic.framework

import breeze.util.Index
import breeze.linalg.DenseVector

/**
 * TODO
 *
 * @author dlwh
 **/
class OneBestInferenceAdaptor[Datum](val inference: AnnotatingInference[Datum]) extends Inference[Datum] {
  type Marginal = inference.Marginal

  def goldMarginal(v: Datum): Marginal = inference.goldMarginal(v)

  def marginal(v: Datum): Marginal = {
    val m = inference.marginal(v)
    goldMarginal(inference.annotate(v, m))
  }

}


class OneBestModelAdaptor[Datum](val model: Model[Datum] { type Inference <: AnnotatingInference[Datum]}) extends Model[Datum] {
  type ExpectedCounts = model.ExpectedCounts
  type Marginal = model.Marginal
  type Inference = OneBestInferenceAdaptor[Datum] { type Marginal = model.Marginal}
  def emptyCounts: ExpectedCounts = model.emptyCounts


  def accumulateCounts(d: Datum, m: Marginal, accum: ExpectedCounts, scale: Double) {
    model.accumulateCounts(d, m, accum, scale)
  }

  def featureIndex: Index[Feature] = model.featureIndex

  def initialValueForFeature(f: Feature): Double = model.initialValueForFeature(f)

  // hack cause i'm lazy.
  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new OneBestInferenceAdaptor[Datum](model.inferenceFromWeights(weights)).asInstanceOf[Inference]

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    model.expectedCountsToObjective(ecounts)
  }
}