package epic.dense

import epic.framework.Feature
import breeze.util.Index
import scala.runtime.ScalaRunTime
import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.numerics._
import epic.features.SegmentedIndex


case class AffineTransform[FV, Mid](numOutputs: Int, numInputs: Int, innerTransform: Transform[FV, Mid], includeBias: Boolean = true)
                              (implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], Mid, DenseVector[Double]],
                               canaxpy: scaleAdd.InPlaceImpl3[DenseVector[Double], Double, Mid]) extends Transform[FV, DenseVector[Double]] {


  val index = SegmentedIndex(new AffineTransform.Index(numOutputs, numInputs, includeBias), innerTransform.index)



  def extractLayer(weights: DenseVector[Double]) = {
    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
    val bias = if(includeBias) {
      weights(numOutputs * numInputs until index.componentOffset(1))
    } else {
      DenseVector.zeros[Double](numOutputs)
    }
    val inner = innerTransform.extractLayer(weights(index.componentOffset(1) to -1))
    new Layer(mat, bias, inner)
  }

  case class Layer(weights: DenseMatrix[Double], bias: DenseVector[Double], innerLayer: innerTransform.Layer) extends _Layer {
    override val index = AffineTransform.this.index

    val weightst = weights.t.copy


    def activations(fv: FV) = {
      val out = weights * innerLayer.activations(fv) += bias
      out
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>DenseVector[Double], fv: FV) = {
      val scale = _scale
      val matDeriv = deriv(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
      val biasDeriv = if(includeBias) {
        deriv(numOutputs * numInputs until index.componentOffset(1))
      } else {
        DenseVector.zeros[Double](numOutputs)
      }

      // whole function is f(mat * inner(fv) + bias)
      // scale(i) pushes in  (f'(mat * inner(v) + bias))(i)
      val innerAct = innerLayer.activations(fv)
      // d/d(weights(::, i)) == scale(i) * innerAct
      for (i <- 0 until weights.rows) {
        val a: Double = scale(i)
        axpy(a, innerAct, matDeriv.t(::, i))
        // so d/dbias(i) = scale(i)
        biasDeriv(i) += a
      }

      // scale is f'(mat * inner(v) + bias)
      // d/dv is mat.t * f'(mat * inner(v) + bias)

      innerLayer.tallyDerivative(deriv(index.componentOffset(1) to -1), weightst * scale, fv)
    }

  }

}

object AffineTransform {
  def typed[FV](numOutputs: Int, numInputs: Int, includeBias: Boolean)(implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], FV, DenseVector[Double]],
                                                                    canAxpy: scaleAdd.InPlaceImpl3[DenseVector[Double], Double, FV]) = new AffineTransform(numOutputs, numInputs, new IdentityTransform[FV], includeBias)
  def apply(numOutputs: Int, numInputs: Int, includeBias: Boolean):AffineTransform[DenseVector[Double], DenseVector[Double]] = apply(numOutputs, numInputs, new IdentityTransform[DenseVector[Double]], includeBias)
  def apply(numOutputs: Int, numInputs: Int):AffineTransform[DenseVector[Double], DenseVector[Double]]  = apply(numOutputs, numInputs, true)
  case class Index(numOutputs: Int, numInputs: Int, includeBias: Boolean = true) extends breeze.util.Index[Feature] {
    def apply(t: Feature): Int = t match {
      case NeuralFeature(output, input) if output < numOutputs && input < numInputs && output > 0 && input > 0 =>
        output * numInputs + input
      case NeuralBias(output) if output <= numOutputs => output + numOutputs * numInputs
      case _ => -1
    }

    def unapply(i: Int): Option[Feature] = {
      if (i < 0 || i >= size) {
        None
      } else if (includeBias && i >= numInputs * numOutputs) {
        Some(NeuralBias(i - numInputs * numOutputs))
      } else  {
        Some(NeuralFeature(i/numInputs, i % numInputs))
      }
    }

    def pairs: Iterator[(Feature, Int)] = iterator zipWithIndex

    def iterator: Iterator[Feature] = Iterator.range(0, size) map (unapply) map (_.get)

    override val size: Int = if(includeBias) numOutputs * numInputs + numOutputs else numOutputs * numInputs

    override def toString() = ScalaRunTime._toString(this)
  }
}