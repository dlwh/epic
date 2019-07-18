package epic.dense

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.numerics._
import epic.framework.Feature
import breeze.util.Index
import scala.util.Random

case class TanhTransform[FV](inner: Transform[FV, DenseVector[Double]]) extends Transform[FV, DenseVector[Double]] {
  def this(numOutputs: Int, numInputs: Int,
           includeBias: Boolean = true)
          (implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], FV, DenseVector[Double]],
           canaxpy: scaleAdd.InPlaceImpl3[DenseVector[Double], Double, FV])  = this(AffineTransform.typed(numOutputs, numInputs, includeBias))

  val index: inner.index.type = inner.index

  def extractLayer(dv: DenseVector[Double], forTrain: Boolean) = new Layer(inner.extractLayer(dv, forTrain))
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = inner.initialWeightVector(initWeightsScale, rng, false, spec)
  
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) = inner.clipHiddenWeightVectors(weights, norm, false)
  
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int] = inner.getInterestingWeightIndicesForGradientCheck(offset)

  case class Layer(innerLayer: inner.Layer) extends Transform.Layer[FV,DenseVector[Double]] {
    
    val myIndex = Index[Feature]
    
    def index = myIndex

    def activations(fv: FV): DenseVector[Double] = {
      val act = innerLayer.activations(fv) * 2.0
      sigmoid.inPlace(act)
      (act :*= 2.0) -= 1.0
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
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
    
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[FV]) = innerLayer.applyBatchNormalization(inputs)

  }

}