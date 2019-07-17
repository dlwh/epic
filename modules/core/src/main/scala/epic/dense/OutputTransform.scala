package epic.dense

import breeze.linalg._
import breeze.util.Index
import epic.framework.Feature
import scala.util.Random

trait OutputTransform[In, +Out] extends Serializable {
  val index: Index[Feature]
  def extractLayer(dv: DenseVector[Double], forTrain: Boolean):OutputLayer = extractLayerAndPenultimateLayer(dv, forTrain)._1
  def extractLayerAndPenultimateLayer(dv: DenseVector[Double], forTrain: Boolean): (OutputLayer, Transform.Layer[In,Out])
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String): DenseVector[Double]
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean)
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int]
  type OutputLayer <: OutputTransform.OutputLayer[In,Out]
}

object OutputTransform {
  
  trait OutputLayer[In, +Out] extends Transform.Layer[In,Out] {
    def index: Index[Feature]
    def activations(fv: In):Out
    def activationsFromPenultimateDot(innerLayerActivations: DenseVector[Double], sparseIdx: Int): Double
    def activationsFromPenultimateDot(innerLayerActivations: DenseVector[Double], sparseFeatures: Array[Int]): Double = {
      var value = 0.0
      for (sparseFeature <- sparseFeatures) {
        value += activationsFromPenultimateDot(innerLayerActivations, sparseFeature)
      }
      value
    }
    def tallyDerivative(deriv: DenseVector[Double], scale: =>Vector[Double], fv: In)
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[In])
  }

}
