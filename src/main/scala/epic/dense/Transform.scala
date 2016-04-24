package epic.dense

import breeze.linalg._
import breeze.util.Index
import epic.framework.Feature
import scala.util.Random

/**
 *
 *
 * @author dlwh
 */
trait Transform[In, +Out] extends Serializable  {
  val index: Index[Feature]
  def extractLayer(dv: DenseVector[Double], forTrain: Boolean):Layer
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String): DenseVector[Double]
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean)
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int]
  type Layer <: Transform.Layer[In,Out]
}

object Transform {
  
  trait Layer[In, +Out] {
    def index: Index[Feature]
    def activations(fv: In):Out
    def tallyDerivative(deriv: DenseVector[Double], scale: =>Vector[Double], fv: In)
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[In])
  }

}

case class NeuralFeature(output: Int, input: Int) extends Feature
case class NeuralBias(input: Int) extends Feature
