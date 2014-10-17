package epic.dense

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.numerics._


case class TanhTransform[FV](inner: Transform[FV, DenseVector[Double]]) extends Transform[FV, DenseVector[Double]] {
  def this(numOutputs: Int, numInputs: Int,
           includeBias: Boolean = true)
          (implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], FV, DenseVector[Double]],
           canaxpy: scaleAdd.InPlaceImpl3[DenseVector[Double], Double, FV])  = this(AffineTransform.typed(numOutputs, numInputs, includeBias))

  val index: inner.index.type = inner.index


  def extractLayer(dv: DenseVector[Double]) = new Layer(inner.extractLayer(dv))

  case class Layer(innerLayer: inner.Layer) extends _Layer {

    def activations(fv: FV): DenseVector[Double] = {
      val act = innerLayer.activations(fv) * 2.0
      sigmoid.inPlace(act)
      (act :*= 2.0) -= 1.0
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>DenseVector[Double], fv: FV) = {
      val scale = _scale
      val act = innerLayer.activations(fv) * 2.0
      sigmoid.inPlace(act)
      // whole function is f(tanh(transform(features))) = f(2 * sigmoid(2 * transform(x)) - 1.0)
      // scale(i) pushes in  (f'(tanh(transform(features)))(i) so just need to finish the chain rule.
      // activations(...) computes 2 * sigmoid(2 * transform(x)) - 1.0
      // d activations / dx = 4 * sigmoid'(2 * transform(x)) * transform'(x)
      //                  = 4 * (sigmoid(2 * transform(x))) * (1-sigmoid(2 * transform(x))) * transform'(x)
      //                  = 2 * (sigmoid(2 * transform(x))) * (1-sigmoid(2 * transform(x))) * transform'(x)
      act :*= (act - 1.0)
      act :*= (-4.0)
      act :*= scale

      innerLayer.tallyDerivative(deriv, act, fv)

    }

  }

}