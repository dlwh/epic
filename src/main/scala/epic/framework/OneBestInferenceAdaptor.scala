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
  type ExpectedCounts = inference.ExpectedCounts

  def emptyCounts: ExpectedCounts = inference.emptyCounts

  def goldMarginal(v: Datum): Marginal = inference.goldMarginal(v)

  def marginal(v: Datum): Marginal = {
    val m = inference.marginal(v)
    goldMarginal(inference.annotate(v, m))
  }

  def countsFromMarginal(v: Datum, marg: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
    inference.countsFromMarginal(v, marg, accum, scale)
  }
}


class OneBestModelAdaptor[Datum](val model: Model[Datum] { type Inference <: AnnotatingInference[Datum]}) extends Model[Datum] {
  type ExpectedCounts = model.ExpectedCounts
  type Marginal = model.Marginal
  type Inference = OneBestInferenceAdaptor[Datum] { type Marginal = model.Marginal; type ExpectedCounts = model.ExpectedCounts}

  def featureIndex: Index[Feature] = model.featureIndex

  def initialValueForFeature(f: Feature): Double = model.initialValueForFeature(f)

  // hack cause i'm lazy.
  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new OneBestInferenceAdaptor[Datum](model.inferenceFromWeights(weights)).asInstanceOf[Inference]

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    model.expectedCountsToObjective(ecounts)
  }
}