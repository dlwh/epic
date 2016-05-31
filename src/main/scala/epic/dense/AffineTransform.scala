package epic.dense

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import epic.features.SegmentedIndex
import epic.framework.Feature

import scala.runtime.ScalaRunTime
import scala.util.Random

case class AffineTransform[FV, Mid](numOutputs: Int, numInputs: Int, innerTransform: Transform[FV, Mid], includeBias: Boolean = true)
                              (implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], Mid, DenseVector[Double]],
                               canaxpy: scaleAdd.InPlaceImpl3[DenseVector[Double], Double, Mid]) extends Transform[FV, DenseVector[Double]] {

  val index = SegmentedIndex(new AffineTransform.Index(numOutputs, numInputs, includeBias), innerTransform.index)

  def extractLayer(weights: DenseVector[Double], forTrain: Boolean) = {
    extractLayerAndPenultimateLayer(weights, forTrain)._1
  }
  
  def extractLayerAndPenultimateLayer(weights: DenseVector[Double], forTrain: Boolean) = {
    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
    val bias = if (includeBias) {
      weights(numOutputs * numInputs until index.componentOffset(1))
    } else {
      DenseVector.zeros[Double](numOutputs)
    }
    val inner = innerTransform.extractLayer(weights(index.componentOffset(1) to -1), forTrain)
    new Layer(mat, bias, inner) -> inner
  }
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = {
//    if (spec == "") {
//    DenseVector(Array.tabulate(index.indices(0).size)(i => if (outputLayer) 0.0 else rng.nextGaussian * initWeightsScale)),
    val myWeights = if (outputLayer) {
      DenseVector(Array.tabulate(index.indices(0).size)(i => 0.0))
    } else if (spec == "magic") {
      AffineTransform.getMagicAffineWeights(index.indices(0).size, numInputs, numOutputs, initWeightsScale, rng)
    } else {
      AffineTransform.getGaussianAffineWeights(index.indices(0).size, initWeightsScale, rng)
    }
    DenseVector.vertcat(myWeights, innerTransform.initialWeightVector(initWeightsScale, rng, false, spec))
  }
  
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) {
    if (!outputLayer) {
      AffineTransform.clipHiddenWeightVectors(numOutputs, numInputs, weights, norm)
    }
    innerTransform.clipHiddenWeightVectors(weights(index.componentOffset(1) to -1), norm, false)
  }
  
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int] = {
    (offset until offset + Math.min(10, index.indices(0).size)) ++ innerTransform.getInterestingWeightIndicesForGradientCheck(offset + index.indices(0).size)
  }

  case class Layer(weights: DenseMatrix[Double], bias: DenseVector[Double], innerLayer: innerTransform.Layer) extends Transform.Layer[FV,DenseVector[Double]] {
    override val index = AffineTransform.this.index

    val weightst = weights.t
    // val weightst = weights.t.copy

    def activations(fv: FV) = {
      val out = weights * innerLayer.activations(fv) += bias
      out
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
    // println("SCALE: " + _scale)
      val scale = _scale
      val matDeriv = deriv(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
      val biasDeriv = if (includeBias) {
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
        if (a != 0.0) {
          axpy(a, innerAct, matDeriv.t(::, i))
        // so d/dbias(i) = scale(i)
          biasDeriv(i) += a
        }
      }

      // biasDeriv += scale

      // scale is f'(mat * inner(v) + bias)
      // d/dv is mat.t * f'(mat * inner(v) + bias)
      // println("Intermediate scale: " + weightst * scale)
      innerLayer.tallyDerivative(deriv(index.componentOffset(1) to -1), weightst * scale, fv)
    }
    
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[FV]) = innerLayer.applyBatchNormalization(inputs)

  }

}

object AffineTransform {
  def typed[FV](numOutputs: Int, numInputs: Int, includeBias: Boolean)(implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], FV, DenseVector[Double]],
                                                                    canAxpy: scaleAdd.InPlaceImpl3[DenseVector[Double], Double, FV]) = new AffineTransform(numOutputs, numInputs, new IdentityTransform[FV], includeBias)
  def apply(numOutputs: Int, numInputs: Int, includeBias: Boolean):AffineTransform[DenseVector[Double], DenseVector[Double]] = apply(numOutputs, numInputs, new IdentityTransform[DenseVector[Double]], includeBias)
  def apply(numOutputs: Int, numInputs: Int):AffineTransform[DenseVector[Double], DenseVector[Double]]  = apply(numOutputs, numInputs, true)

  def getUniformAffineWeights(numWeights: Int, initWeightsScale: Double, rng: Random) = {
    DenseVector(Array.tabulate(numWeights)(i => rng.nextGaussian * initWeightsScale))
  }
  
  def getGaussianAffineWeights(numWeights: Int, initWeightsScale: Double, rng: Random) = {
    DenseVector(Array.tabulate(numWeights)(i => rng.nextGaussian * initWeightsScale))
  }
  
  // N.B. numWeights != inSize * outSize if there's a bias
  def getMagicAffineWeights(numWeights: Int, inSize: Int, outSize: Int, initWeightsScale: Double, rng: Random) = {
    val range = Math.sqrt(6.0/(inSize + outSize))
    DenseVector(Array.tabulate(numWeights)(i => rng.nextDouble * 2 * range - range))
  }
  
  def clipHiddenWeightVectors(numOutputs: Int, numInputs: Int, weights: DenseVector[Double], norm: Double) {
    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
    for (i <- 0 until mat.rows) {
      val thisRowNorm = breeze.linalg.norm(mat(i, ::), 2)
      val multFactor = norm/Math.sqrt(thisRowNorm)
      mat(i, ::) *= multFactor
    }
  }
  
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

    def makeMatrix(dv: DenseVector[Double]):DenseMatrix[Double] = {
      assert(dv.stride == 1)
      new DenseMatrix(numOutputs, numInputs, dv.data, dv.offset)
    }

    def pairs: Iterator[(Feature, Int)] = iterator.zipWithIndex

    def iterator: Iterator[Feature] = Iterator.range(0, size) map unapply map (_.get)

    override val size: Int = if (includeBias) numOutputs * numInputs + numOutputs else numOutputs * numInputs

    override def toString() = ScalaRunTime._toString(this)
  }
}