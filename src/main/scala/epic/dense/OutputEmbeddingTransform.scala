package epic.dense

import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import epic.features.SegmentedIndex
import epic.framework.Feature

import scala.runtime.ScalaRunTime
import scala.util.Random

case class OutputEmbeddingTransform[FV](numOutputs: Int, outputDim: Int, innerTransform: Transform[FV, DenseVector[Double]], coarsenerForInitialization: Option[Int => Int] = None) extends OutputTransform[FV, DenseVector[Double]] {


  val index = SegmentedIndex(new AffineTransform.Index(numOutputs, outputDim, true),
                             innerTransform.index)
  
  def extractLayerAndPenultimateLayer(weights: DenseVector[Double], forTrain: Boolean) = {
    val embeddings = weights(index.componentOffset(0) until index.componentOffset(0) + (numOutputs * outputDim)).asDenseMatrix.reshape(numOutputs, outputDim, view = View.Require)
    val bias = weights(index.componentOffset(0) + numOutputs * outputDim until index.componentOffset(1))
    val inner = innerTransform.extractLayer(weights(index.componentOffset(1) to -1), forTrain)
    new OutputLayer(embeddings, bias, inner) -> inner
  }
  
  def clipEmbeddingNorms(weights: DenseVector[Double]) {
    val embeddings = weights(index.componentOffset(1) until index.componentOffset(1) + (numOutputs * outputDim)).asDenseMatrix.reshape(numOutputs, outputDim, view = View.Require)
    AffineOutputEmbeddingTransform.clipEmbeddingNorms(embeddings);
  }
  
  def displayEmbeddingNorms(weights: DenseVector[Double]) {
    val embeddings = weights(index.componentOffset(1) until index.componentOffset(1) + (numOutputs * outputDim)).asDenseMatrix.reshape(numOutputs, outputDim, view = View.Require)
    AffineOutputEmbeddingTransform.displayEmbeddingNorms(embeddings);
  }
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = {
    require(outputLayer)
    val embeddingsInitialization = if (coarsenerForInitialization.isDefined) {
      OutputEmbeddingTransform.getCoarsenedInitialEmbeddingWeights(numOutputs, outputDim, coarsenerForInitialization.get)
    } else if (spec == "magic") {
      AffineTransform.getMagicAffineWeights(index.indices(1).size, numOutputs, outputDim, initWeightsScale, rng)
    } else if (spec == "identity") {
      AffineOutputEmbeddingTransform.getIdentityEmbeddingWeights(numOutputs, outputDim, rng)
    } else {
      AffineTransform.getGaussianAffineWeights(index.indices(1).size, initWeightsScale, rng)
    }
    // N.B. "true" because the next layer effectively becomes the output layer from the purposes of
    // initialization
    DenseVector.vertcat(embeddingsInitialization,
                        innerTransform.initialWeightVector(initWeightsScale, rng, true, spec))
  }
  
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) {
    innerTransform.clipHiddenWeightVectors(weights(index.componentOffset(1) to -1), norm, false)
  }

  case class OutputLayer(embeddings: DenseMatrix[Double], bias: DenseVector[Double], innerLayer: innerTransform.Layer) extends OutputTransform.OutputLayer[FV,DenseVector[Double]] {
    override val index = OutputEmbeddingTransform.this.index

    def activations(fv: FV) = {
      val innerActs = innerLayer.activations(fv)
      DenseVector(Array.tabulate(numOutputs)(i => activationsFromPenultimateDot(innerActs, i)))
    }
    
    def activationsFromPenultimateDot(innerLayerActivations: DenseVector[Double], sparseIdx: Int) = {
      innerLayerActivations dot embeddings(sparseIdx, ::).t + bias(sparseIdx)
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
      val scale = _scale
      val embeddingsDeriv = deriv(0 until numOutputs * outputDim).asDenseMatrix.reshape(numOutputs, outputDim, view = View.Require)
      val biasDeriv = deriv(numOutputs * outputDim until index.componentOffset(1))
      val innerAct = innerLayer.activations(fv)
      val innerScale = DenseVector(Array.tabulate(outputDim)(i => 0.0))
      // TODO: Compute derivatives here
      for (k <- 0 until scale.size) {
        // Assuming there's something nontrivial to pass back
        if (scale(k) != 0.0) {
          // Bias update
          biasDeriv(k) += scale(k)
          // Dumb way
//          for (i <- 0 until numInputs) {
//            for (j <- 0 until outputDim) {
//              weightsDeriv(j, i) += scale(k) * innerAct(i) * embeddings(k, j)
//              embeddingsDeriv(k, j) += scale(k) * weights(j, i) * innerAct(i)
//              innerScale(i) += scale(k) * weights(j, i) * embeddings(k, j)
//            }
//          }
          // Smart way; matches the dumb way exactly
          // Column * row gives outer product
          // TODO: Derive update here
          embeddingsDeriv(k, ::).t += innerAct * scale(k) // Embeddings update
          innerScale += embeddings(k, ::).t * scale(k)
//          weightsDeriv += embeddings(k, ::).t * innerAct.t * scale(k) // Weights update
//          embeddingsDeriv(k, ::).t += weights * innerAct * scale(k) // Embeddings update
//          innerScale += weightst * embeddings(k, ::).t * scale(k) // Inner scale update
        }
      }
      innerLayer.tallyDerivative(deriv(index.componentOffset(1) to -1), innerScale, fv)
    }
    
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[FV]) = innerLayer.applyBatchNormalization(inputs)
  }

}

object OutputEmbeddingTransform {
  
  def getCoarsenedInitialEmbeddingWeights(numOutputs: Int, outputDim: Int, coarsenerForInitialization: Int => Int) = {
    val mat = DenseMatrix.zeros[Double](numOutputs, outputDim)
    for (i <- 0 until numOutputs) {
      val j = ((coarsenerForInitialization(i) % outputDim) + outputDim) % outputDim
      mat(i, j) = 1.0
    }
    val biasInitializer = DenseVector.zeros[Double](numOutputs)
    val initWeights = DenseVector.vertcat(DenseVector(mat.data), biasInitializer)
    initWeights
  }
}
