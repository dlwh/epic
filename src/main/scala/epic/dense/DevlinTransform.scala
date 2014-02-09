package epic.dense

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.linalg.support.CanAxpy
import epic.features.SegmentedIndex
import breeze.util.Index
import breeze.features.FeatureVector
import epic.framework.Feature


case class DevlinTransform[L](inputIndex: Index[L], embedDim: Int) extends Transform[FeatureVector, DenseVector[Double]] {


  val index = new AffineTransform.Index(embedDim, inputIndex.size, false)

  // TODO: add bias term
  def extractLayer(weights: DenseVector[Double]) = {
    val mat = weights.asDenseMatrix.reshape(embedDim, inputIndex.size)
    new Layer(mat)
  }

  case class Layer(weights: DenseMatrix[Double]) extends _Layer {
    override val index = DevlinTransform.this.index

    def activations(fv: FeatureVector) = {
      val out = DenseVector.vertcat(fv.data.map(weights(::, _)):_*)
      out
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>DenseVector[Double], fv: FeatureVector) = {
      val reshapedDeriv = deriv.asDenseMatrix.reshape(embedDim, inputIndex.size)
      val scale = _scale
      for(pos <- 0 until fv.data.length) {
        val w = fv.data(pos)
        reshapedDeriv(::, w) += scale(pos * embedDim until (pos+1) * embedDim)
      }

    }

  }

}


