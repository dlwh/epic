package epic.everything.models

import breeze.linalg.DenseVector
import epic.everything.DocumentAnnotator
import epic.framework.{StandardExpectedCounts, Feature}
import breeze.util.{Encoder, Index}
import breeze.sequences.CRF

/**
 *
 * @author dlwh
class ChainNERModel(val featureIndex: Index[Feature],
                    featurizer: CrfFeaturizer) extends DocumentAnnotatingModel {
  type ExpectedCounts = StandardExpectedCounts

  def initialValueForFeature(f: Feature): Double = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new ChainNERInference(weights, featureIndex, featurizer)

  def emptyCounts: ExpectedCounts = StandardExpectedCounts.zero(featureIndex)

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    StandardExpectedCounts.unapply(ecounts).get
  }

  type Inference = ChainNERInference

  def annotator(weights: DenseVector[Double]): DocumentAnnotator = 10
}

case class ChainNERInference(weights: DenseVector[Double], index: Index[Feature], featurizer: CRFFeaturizer) {

}
 */

