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
case class AffineTransformDense[FV](numOutputs: Int, numInputs: Int, innerTransform: Transform[FV, DenseVector[Double]], includeBias: Boolean = true) extends Transform[FV, DenseVector[Double]] {


  val index = SegmentedIndex(new AffineTransformDense.Index(numOutputs, numInputs, includeBias), innerTransform.index)

  def extractLayer(weights: DenseVector[Double]) = {
    extractLayerAndPenultimateLayer(weights)._1
  }
  
  def extractLayerAndPenultimateLayer(weights: DenseVector[Double]) = {
    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
    val bias = if(includeBias) {
      weights(numOutputs * numInputs until index.componentOffset(1))
    } else {
      DenseVector.zeros[Double](numOutputs)
    }
    val inner = innerTransform.extractLayer(weights(index.componentOffset(1) to -1))
    new Layer(mat, bias, inner) -> inner
  }
  
  /**
   * N.B. Initialized to zero because this should *only* be used at the output layer, where
   * zero initialization is appropriate
   */
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean) = {
    require(outputLayer)
    DenseVector.vertcat(DenseVector.zeros(index.indices(0).size), innerTransform.initialWeightVector(initWeightsScale, rng, false))
  }

  case class Layer(weights: DenseMatrix[Double], bias: DenseVector[Double], innerLayer: innerTransform.Layer) extends Transform.Layer[FV,DenseVector[Double]] {
    override val index = AffineTransformDense.this.index

    val weightst = weights.t
//    val weightst = weights.t.copy


    def activations(fv: FV) = {
      val out = weights * innerLayer.activations(fv) += bias
      out
    }
    
    def activationsFromPenultimateDot(innerLayerActivations: DenseVector[Double], sparseFeatures: Array[Int]) = {
      var value = 0.0;
      for (sparseFeature <- sparseFeatures) {
        value += (weights(sparseFeature, ::) * innerLayerActivations + bias(sparseFeature))
      }
      value
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
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
        if(a != 0.0) {
          axpy(a, innerAct, matDeriv.t(::, i))
        // so d/dbias(i) = scale(i)
          biasDeriv(i) += a
        }
      }

//      biasDeriv += scale

      // scale is f'(mat * inner(v) + bias)
      // d/dv is mat.t * f'(mat * inner(v) + bias)

      innerLayer.tallyDerivative(deriv(index.componentOffset(1) to -1), weightst * scale, fv)
    }

  }

}

object AffineTransformDense {
//  def typed[FV](numOutputs: Int, numInputs: Int, includeBias: Boolean)(implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], FV, DenseVector[Double]],
//                                                                    canAxpy: scaleAdd.InPlaceImpl3[DenseVector[Double], Double, FV]) = new AffineTransformDense(numOutputs, numInputs, new IdentityTransform[FV], includeBias)
  def apply(numOutputs: Int, numInputs: Int, includeBias: Boolean):AffineTransformDense[DenseVector[Double]] = apply(numOutputs, numInputs, new IdentityTransform[DenseVector[Double]], includeBias)
  def apply(numOutputs: Int, numInputs: Int):AffineTransformDense[DenseVector[Double]]  = apply(numOutputs, numInputs, true)
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

    override val size: Int = if(includeBias) numOutputs * numInputs + numOutputs else numOutputs * numInputs

    override def toString() = ScalaRunTime._toString(this)
  }
}