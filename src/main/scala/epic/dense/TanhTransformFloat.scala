//package epic.dense
//
//import breeze.linalg._
//import breeze.linalg.operators.OpMulMatrix
//import breeze.numerics._
//
//
//case class TanhTransformFloat[FV](inner: TransformFloat[FV, DenseVector[Float]]) extends TransformFloat[FV, DenseVector[Float]] {
//  def this(numOutputs: Int, numInputs: Int,
//           includeBias: Boolean = true)
//          (implicit mult: OpMulMatrix.Impl2[DenseMatrix[Float], FV, DenseVector[Float]],
//           canaxpy: scaleAdd.InPlaceImpl3[DenseVector[Float], Float, FV])  = this(AffineTransformFloat.typed(numOutputs, numInputs, includeBias))
//
//  val index: inner.index.type = inner.index
//
//
//  def extractLayer(dv: DenseVector[Float]) = new Layer(inner.extractLayer(dv))
//
//  case class Layer(innerLayer: inner.Layer) extends _Layer {
//
//    def activations(fv: FV): DenseVector[Float] = {
//      val act = innerLayer.activations(fv) * 2.0F
//      var i = 0
//      while (i < act.size) {
//        act(i) = (1.0/(1.0 + Math.exp(act(i)))).toFloat
//        i += 1
//      }
//      (act :*= 2.0F) -= 1.0F
//    }
//
//    def tallyDerivative(deriv: DenseVector[Float], _scale: =>Vector[Float], fv: FV) = {
//      val scale = _scale
//      val act = innerLayer.activations(fv) * 2.0F
//      var i = 0
//      while (i < act.size) {
//        act(i) = (1.0/(1.0 + Math.exp(act(i)))).toFloat
//        i += 1
//      }
//      // whole function is f(tanh(transform(features))) = f(2 * sigmoid(2 * transform(x)) - 1.0)
//      // scale(i) pushes in  (f'(tanh(transform(features)))(i) so just need to finish the chain rule.
//      // activations(...) computes 2 * sigmoid(2 * transform(x)) - 1.0
//      // d activations / dx = 4 * sigmoid'(2 * transform(x)) * transform'(x)
//      //                  = 4 * (sigmoid(2 * transform(x))) * (1-sigmoid(2 * transform(x))) * transform'(x)
//      //                  = 2 * (sigmoid(2 * transform(x))) * (1-sigmoid(2 * transform(x))) * transform'(x)
//      act :*= (act - 1.0F)
//      act :*= (-4.0F)
//      act :*= scale
//
//      innerLayer.tallyDerivative(deriv, act, fv)
//
//    }
//
//  }
//
//}