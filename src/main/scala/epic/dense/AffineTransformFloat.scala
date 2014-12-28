//package epic.dense
//
//import breeze.linalg._
//import breeze.linalg.operators.OpMulMatrix
//import epic.features.SegmentedIndex
//import epic.framework.Feature
//
//import scala.runtime.ScalaRunTime
//
//
//case class AffineTransformFloat[FV, Mid](numOutputs: Int, numInputs: Int, innerTransform: TransformFloat[FV, Mid], includeBias: Boolean = true)
//                              (implicit mult: OpMulMatrix.Impl2[DenseMatrix[Float], Mid, DenseVector[Float]],
//                               canaxpy: scaleAdd.InPlaceImpl3[DenseVector[Float], Float, Mid]) extends TransformFloat[FV, DenseVector[Float]] {
//
//
//  val index = SegmentedIndex(new AffineTransformFloat.Index(numOutputs, numInputs, includeBias), innerTransform.index)
//
//
//
//  def extractLayer(weights: DenseVector[Float]) = {
//    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
//    val bias = if(includeBias) {
//      weights(numOutputs * numInputs until index.componentOffset(1))
//    } else {
//      DenseVector.zeros[Float](numOutputs)
//    }
//    val inner = innerTransform.extractLayer(weights(index.componentOffset(1) to -1))
//    new Layer(mat, bias, inner)
//  }
//
//  case class Layer(weights: DenseMatrix[Float], bias: DenseVector[Float], innerLayer: innerTransform.Layer) extends _Layer {
//    override val index = AffineTransformFloat.this.index
//
////    val weightst = weights.t.copy
//    val weightst = weights.t
//
//    def activations(fv: FV) = {
//      val out = weights * innerLayer.activations(fv) += bias
//      out
//    }
//
//    def tallyDerivative(deriv: DenseVector[Float], _scale: =>Vector[Float], fv: FV) = {
//      val scale = _scale
//      val matDeriv = deriv(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
//      val biasDeriv = if(includeBias) {
//        deriv(numOutputs * numInputs until index.componentOffset(1))
//      } else {
//        DenseVector.zeros[Float](numOutputs)
//      }
//
//      // whole function is f(mat * inner(fv) + bias)
//      // scale(i) pushes in  (f'(mat * inner(v) + bias))(i)
//      val innerAct = innerLayer.activations(fv)
//      // d/d(weights(::, i)) == scale(i) * innerAct
//      for (i <- 0 until weights.rows) {
//        val a: Float = scale(i)
//        if(a != 0.0) {
//          axpy(a, innerAct, matDeriv.t(::, i))
//        // so d/dbias(i) = scale(i)
//          biasDeriv(i) += a
//        }
//      }
//
////      biasDeriv += scale
//
//      // scale is f'(mat * inner(v) + bias)
//      // d/dv is mat.t * f'(mat * inner(v) + bias)
//
//      innerLayer.tallyDerivative(deriv(index.componentOffset(1) to -1), weightst * scale, fv)
//    }
//
//  }
//
//}
//
//object AffineTransformFloat {
//  def typed[FV](numOutputs: Int, numInputs: Int, includeBias: Boolean)(implicit mult: OpMulMatrix.Impl2[DenseMatrix[Float], FV, DenseVector[Float]],
//                                                                    canAxpy: scaleAdd.InPlaceImpl3[DenseVector[Float], Float, FV]) = new AffineTransformFloat(numOutputs, numInputs, new IdentityTransformFloat[FV], includeBias)
//  def apply(numOutputs: Int, numInputs: Int, includeBias: Boolean):AffineTransformFloat[DenseVector[Float], DenseVector[Float]] = apply(numOutputs, numInputs, new IdentityTransformFloat[DenseVector[Float]], includeBias)
//  def apply(numOutputs: Int, numInputs: Int):AffineTransformFloat[DenseVector[Float], DenseVector[Float]]  = apply(numOutputs, numInputs, true)
//  case class Index(numOutputs: Int, numInputs: Int, includeBias: Boolean = true) extends breeze.util.Index[Feature] {
//    def apply(t: Feature): Int = t match {
//      case NeuralFeature(output, input) if output < numOutputs && input < numInputs && output > 0 && input > 0 =>
//        output * numInputs + input
//      case NeuralBias(output) if output <= numOutputs => output + numOutputs * numInputs
//      case _ => -1
//    }
//
//    def unapply(i: Int): Option[Feature] = {
//      if (i < 0 || i >= size) {
//        None
//      } else if (includeBias && i >= numInputs * numOutputs) {
//        Some(NeuralBias(i - numInputs * numOutputs))
//      } else  {
//        Some(NeuralFeature(i/numInputs, i % numInputs))
//      }
//    }
//
//    def makeMatrix(dv: DenseVector[Float]):DenseMatrix[Float] = {
//      assert(dv.stride == 1)
//      new DenseMatrix(numOutputs, numInputs, dv.data, dv.offset)
//    }
//
//    def pairs: Iterator[(Feature, Int)] = iterator.zipWithIndex
//
//    def iterator: Iterator[Feature] = Iterator.range(0, size) map unapply map (_.get)
//
//    override val size: Int = if(includeBias) numOutputs * numInputs + numOutputs else numOutputs * numInputs
//
//    override def toString() = ScalaRunTime._toString(this)
//  }
//}