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
trait OutputTransform[In, +Out] {
  val index: Index[Feature]

  def extractLayer(dv: DenseVector[Double]):OutputLayer = extractLayerAndPenultimateLayer(dv)._1
  
//  def extractLayerAndPenultimateLayer(dv: DenseVector[Double]): (OutputLayer, Transform[In,Out]#Layer);
  def extractLayerAndPenultimateLayer(dv: DenseVector[Double]): (OutputLayer, Transform.Layer[In,Out]);
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String): DenseVector[Double]

  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean)
  
  type OutputLayer <: OutputTransform.OutputLayer[In,Out]
}

object OutputTransform {
  
  trait OutputLayer[In, +Out] extends Transform.Layer[In,Out] {

    def index: Index[Feature];

    def activations(fv: In):Out
    
    def activationsFromPenultimateDot(innerLayerActivations: DenseVector[Double], sparseIdx: Int): Double;
    
    def activationsFromPenultimateDot(innerLayerActivations: DenseVector[Double], sparseFeatures: Array[Int]): Double = {
      var value = 0.0;
      for (sparseFeature <- sparseFeatures) {
        value += activationsFromPenultimateDot(innerLayerActivations, sparseFeature)
      }
      value
    }

    def tallyDerivative(deriv: DenseVector[Double], scale: =>Vector[Double], fv: In)

  }

}
