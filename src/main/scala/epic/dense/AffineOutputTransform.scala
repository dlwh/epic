package epic.dense

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import epic.features.SegmentedIndex
import epic.framework.Feature

import scala.runtime.ScalaRunTime
import scala.util.Random

/**
 * Used at the output layer when we're only going to need some of the possible ouputs;
 * it exposes the penultimate layer and then the Layer allows you to pass the results
 * from that back in (caching it elsewhere) and only compute certain cells in the
 * output layer (activationsFromPenultimateDot). 
 */
case class AffineOutputTransform[FV](numOutputs: Int, numInputs: Int, innerTransform: Transform[FV, DenseVector[Double]], includeBias: Boolean = true) extends OutputTransform[FV, DenseVector[Double]] {

  val index = SegmentedIndex(new AffineTransform.Index(numOutputs, numInputs, includeBias), innerTransform.index)
  
  def extractLayerAndPenultimateLayer(weights: DenseVector[Double], forTrain: Boolean) = {
    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
    val bias = if (includeBias) {
      weights(numOutputs * numInputs until index.componentOffset(1))
    } else {
      DenseVector.zeros[Double](numOutputs)
    }
    val inner = innerTransform.extractLayer(weights(index.componentOffset(1) to -1), forTrain)
    new OutputLayer(mat, bias, inner) -> inner
  }
  
  /**
   * N.B. Initialized to zero because this should *only* be used at the output layer, where
   * zero initialization is appropriate
   */
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = {
    require(outputLayer)
    DenseVector.vertcat(DenseVector.zeros(index.indices(0).size), innerTransform.initialWeightVector(initWeightsScale, rng, false, spec))
  }
  
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) {
    innerTransform.clipHiddenWeightVectors(weights(index.componentOffset(1) to -1), norm, false)
  }
  
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int] = {
    (offset until offset + Math.min(10, index.indices(0).size)) ++ innerTransform.getInterestingWeightIndicesForGradientCheck(offset + index.indices(0).size)
  }

  case class OutputLayer(weights: DenseMatrix[Double], bias: DenseVector[Double], innerLayer: innerTransform.Layer) extends OutputTransform.OutputLayer[FV,DenseVector[Double]] {
    override val index = AffineOutputTransform.this.index

    val weightst = weights.t
    //val weightst = weights.t.copy

    def activations(fv: FV) = {
      val out = weights * innerLayer.activations(fv) += bias
      out
    }
    
    def activationsDot(fv: FV, sparseIdx: Int) = {
      activationsFromPenultimateDot(innerLayer.activations(fv), sparseIdx)
    }
    
    def activationsDot(fv: FV, sparseIndices: Array[Int]) = {
      activationsFromPenultimateDot(innerLayer.activations(fv), sparseIndices)
    }
    
    def activationsFromPenultimateDot(innerLayerActivations: DenseVector[Double], sparseIdx: Int) = {
      weights(sparseIdx, ::) * innerLayerActivations + bias(sparseIdx)
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
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

      // scale is f'(mat * inner(v) + bias)
      // d/dv is mat.t * f'(mat * inner(v) + bias)

      innerLayer.tallyDerivative(deriv(index.componentOffset(1) to -1), weightst * scale, fv)
    }
    
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[FV]) = innerLayer.applyBatchNormalization(inputs)

  }

}
