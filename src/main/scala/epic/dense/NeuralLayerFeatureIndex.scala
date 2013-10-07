package epic.dense

import epic.framework.Feature
import breeze.util.Index
import scala.runtime.ScalaRunTime
import breeze.linalg._
import breeze.linalg.operators.{CanAxpy, OpMulMatrix, BinaryOp}
import breeze.numerics._

/**
 *
 *
 * @author dlwh
 */
case class NeuralFeature(output: Int, input: Int) extends Feature
case class NeuralBias(input: Int) extends Feature

case class NeuralLayerFeatureIndex(numOutputs: Int, numInputs: Int, includeBias: Boolean = true) extends Index[Feature] {
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

  def extractLayer(weights: DenseVector[Double]) = {
    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
    val bias = if(includeBias) {
      Some(weights(numOutputs * numInputs until (size)))
    } else {
      None
    }
    new NeuralLayer(mat, bias)
  }

  case class NeuralLayer(weights: DenseMatrix[Double], bias: Option[DenseVector[Double]]) {
    val index = NeuralLayerFeatureIndex.this

    def activations[FV](fv: FV)(implicit mult: BinaryOp[DenseMatrix[Double], FV, OpMulMatrix, DenseVector[Double]]) = {
      val out = weights * fv
      bias foreach { out += _}
      sigmoid.inPlace(out)
      out
    }

    def tallyDerivative[FV](deriv: DenseVector[Double],
                            scale: DenseVector[Double],
                            fv: FV)
                           (implicit mult: BinaryOp[DenseMatrix[Double], FV, OpMulMatrix, DenseVector[Double]],
                            canaxpy: CanAxpy[Double, FV, DenseVector[Double]]) = {
      val NeuralLayer(matDeriv, biasDeriv) = extractLayer(deriv)
      val act: DenseVector[Double] = activations(fv)
      act :*= (act - 1.0)
      act :*= -1.0

      // whole function is f(sigmoid(mat * features + bias))
      // scale(i) pushes in  (f'(sigmoid(mat * features + bias)))(i) so just need to finish the chain rule.
      // activations(...) computes sigmoid(mat * features + bias)
      // act is currently sigmoid'(mat * features + bias)
      // so d/dbias(i) = act(i) * scale(i)
      // d/d(weights(output, ::)) = act(i) * scale(i) * fv
      for (i <- 0 until weights.rows) {
        val a: Double = scale(i) * act(i)
        axpy(a, fv, matDeriv.t(::, i))
        biasDeriv.foreach(_(i) += a)
      }
    }

  }

}