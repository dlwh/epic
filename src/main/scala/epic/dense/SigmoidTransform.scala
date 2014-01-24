package epic.dense

import epic.framework.Feature
import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.numerics._
import breeze.linalg.support.{CanMapValues, CanAxpy}

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
           canaxpy: CanAxpy[Double, FV, DenseVector[Double]])  = this(AffineTransform.typed(numOutputs, numInputs, includeBias))

  val index: inner.index.type = inner.index


  def extractLayer(dv: DenseVector[Double]) = new Layer(inner.extractLayer(dv))

  case class Layer(innerLayer: inner.Layer) extends _Layer {
    val index = SigmoidTransform.this

    def activations(fv: FV): DenseVector[Double] = sigmoid(innerLayer.activations(fv))

    def tallyDerivative(deriv: DenseVector[Double], scale: DenseVector[Double], fv: FV) = {
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