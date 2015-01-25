package epic.dense

import epic.framework.Feature
import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.numerics._
import breeze.linalg.support.{CanMapValues}
import breeze.util.Index
import scala.util.Random

/**
 *
 *
 * @author dlwh
 */
case class NeuralFeature(output: Int, input: Int) extends Feature
case class NeuralBias(input: Int) extends Feature

case class SigmoidTransform[FV](inner: Transform[FV, DenseVector[Double]]) extends Transform[FV, DenseVector[Double]] {
  def this(numOutputs: Int, numInputs: Int,
           includeBias: Boolean = true)
          (implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], FV, DenseVector[Double]],
           canaxpy: scaleAdd.InPlaceImpl3[DenseVector[Double], Double, FV])  = this(AffineTransform.typed(numOutputs, numInputs, includeBias))

  val index: inner.index.type = inner.index


  def extractLayer(dv: DenseVector[Double]) = new Layer(inner.extractLayer(dv))
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = inner.initialWeightVector(initWeightsScale, rng, false, spec)

  case class Layer(innerLayer: inner.Layer) extends Transform.Layer[FV,DenseVector[Double]] {

    val myIndex = Index[Feature]
    
    def index = myIndex;
    
    def activations(fv: FV): DenseVector[Double] = sigmoid(innerLayer.activations(fv))

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
      val scale = _scale
      val act = activations(fv)
      act :*= (act - 1.0)
      act :*= -1.0
      // whole function is f(sigmoid(transform(features)))
      // scale(i) pushes in  (f'(sigmoid(transform(features)))(i) so just need to finish the chain rule.
      // activations(...) computes sigmoid(transform(features))
      // act is currently sigmoid'(transform(...))
      act :*= scale

      innerLayer.tallyDerivative(deriv, act, fv)

    }

  }

}