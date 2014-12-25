package epic.dense

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.numerics._
import epic.framework.Feature
import breeze.util.Index


case class CubeTransform[FV](inner: Transform[FV, DenseVector[Double]]) extends Transform[FV, DenseVector[Double]] {

  val index: inner.index.type = inner.index

  def extractLayer(dv: DenseVector[Double]) = new Layer(inner.extractLayer(dv))

  case class Layer(innerLayer: inner.Layer) extends Transform.Layer[FV,DenseVector[Double]] {
    
    val myIndex = Index[Feature]
    
    def index = myIndex;

    def activations(fv: FV): DenseVector[Double] = {
      val act = innerLayer.activations(fv)
      var i = 0;
      while (i < act.size) {
        act(i) = act(i) * act(i) * act(i)
        i += 1
      }
      act
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
      val scale = _scale
      
      // whole function is f(relu(transform(features)))
      // scale(i) pushes in f'(relu(transform(features)))(i) so just need to finish the chain rule.
      // f(x) = max(x, 0)
      // df/dx = if (x > 0) 1 else 0
      val act = innerLayer.activations(fv)
      var i = 0;
      while (i < act.size) {
        act(i) = 3 * act(i) * act(i)
        i += 1
      }
      act :*= scale
      innerLayer.tallyDerivative(deriv, act, fv)
    }

  }

}