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
  type Scorer = inference.Scorer

  def scorer(v: Datum): Scorer = inference.scorer(v)

  def goldMarginal(scorer: Scorer, v: Datum): Marginal = inference.goldMarginal(scorer, v)

  /**
   * Produces the "guess marginal" which is the marginal conditioned on only the input data
   * @param v the example
   * @return gold marginal
   */
  def marginal(scorer: Scorer, v: Datum): Marginal = {
    val m = inference.marginal(scorer, v)
    goldMarginal(scorer, inference.annotate(v, m))
  }

}

class OneBestModelAdaptor[Datum](val model: Model[Datum] { type Inference <: AnnotatingInference[Datum]}) extends Model[Datum] {
  type ExpectedCounts = model.ExpectedCounts
  type Marginal = model.Marginal
  type Scorer = model.Scorer
  type Inference = OneBestInferenceAdaptor[Datum] { type Marginal = model.Marginal; type Scorer = model.Scorer}
  def emptyCounts: ExpectedCounts = model.emptyCounts

  def accumulateCounts(inf: Inference, s: Scorer, d: Datum, m: Marginal, accum: ExpectedCounts, scale: Double) {
    model.accumulateCounts(inf.inference.asInstanceOf[model.Inference], s, d, m, accum, scale)
  }

  def featureIndex: Index[Feature] = model.featureIndex

  def initialValueForFeature(f: Feature): Double = model.initialValueForFeature(f)

  // hack cause i'm lazy.
  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new OneBestInferenceAdaptor[Datum](model.inferenceFromWeights(weights)).asInstanceOf[Inference]

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    model.expectedCountsToObjective(ecounts)
  }
}