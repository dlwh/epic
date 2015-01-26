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
trait Transform[In, +Out] {
  val index: Index[Feature]


  def extractLayer(dv: DenseVector[Double]):Layer
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String): DenseVector[Double]

  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean)
  
  type Layer <: Transform.Layer[In,Out]
}

object Transform {
  
  trait Layer[In, +Out] {

    def index: Index[Feature];

    def activations(fv: In):Out

    def tallyDerivative(deriv: DenseVector[Double], scale: =>Vector[Double], fv: In)

  }

}
