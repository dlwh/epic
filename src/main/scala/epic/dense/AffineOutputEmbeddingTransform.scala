//package epic.dense
//
//import breeze.linalg._
//import breeze.linalg.operators.OpMulMatrix
//import epic.features.SegmentedIndex
//import epic.framework.Feature
//
//import scala.runtime.ScalaRunTime
//import scala.util.Random
//
///**
// * Used at the output layer when we're only going to need some of the possible ouputs;
// * it exposes the penultimate layer and then the Layer allows you to pass the results
// * from that back in (caching it elsewhere) and only compute certain cells in the
// * output layer (activationsFromPenultimateDot). 
// */
// XXX: Commented out because you should just combine OutputEmbeddingTransform + AffineTransform regularly...
//case class AffineOutputEmbeddingTransform[FV](numOutputs: Int, numInputs: Int, outputDim: Int, innerTransform: Transform[FV, DenseVector[Double]]) extends OutputTransform[FV, DenseVector[Double]] {
//
//
//  val index = SegmentedIndex(new AffineTransform.Index(outputDim, numInputs, false),
//                             new AffineTransform.Index(numOutputs, outputDim, true),
//                             innerTransform.index)
//  
//  def extractLayerAndPenultimateLayer(weights: DenseVector[Double], forTrain: Boolean) = {
//    val mat = weights(0 until (outputDim * numInputs)).asDenseMatrix.reshape(outputDim, numInputs, view = View.Require)
//    val embeddings = weights(index.componentOffset(1) until index.componentOffset(1) + (numOutputs * outputDim)).asDenseMatrix.reshape(numOutputs, outputDim, view = View.Require)
//    val bias = weights(index.componentOffset(1) + numOutputs * outputDim until index.componentOffset(2))
//    val inner = innerTransform.extractLayer(weights(index.componentOffset(2) to -1), forTrain)
//    new OutputLayer(mat, embeddings, bias, inner) -> inner
//  }
//  
//  def clipEmbeddingNorms(weights: DenseVector[Double]) {
//    val embeddings = weights(index.componentOffset(1) until index.componentOffset(1) + (numOutputs * outputDim)).asDenseMatrix.reshape(numOutputs, outputDim, view = View.Require)
//    AffineOutputEmbeddingTransform.clipEmbeddingNorms(embeddings);
//  }
//  
//  def displayEmbeddingNorms(weights: DenseVector[Double]) {
//    val embeddings = weights(index.componentOffset(1) until index.componentOffset(1) + (numOutputs * outputDim)).asDenseMatrix.reshape(numOutputs, outputDim, view = View.Require)
//    AffineOutputEmbeddingTransform.displayEmbeddingNorms(embeddings);
//  }
//  
//  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = {
//    require(outputLayer)
//    val embeddingsInitialization = if (spec == "magic") {
//      AffineTransform.getMagicAffineWeights(index.indices(1).size, numOutputs, outputDim, initWeightsScale, rng)
//    } else if (spec == "identity") {
//      AffineOutputEmbeddingTransform.getIdentityEmbeddingWeights(numOutputs, outputDim, rng)
//    } else {
//      AffineTransform.getGaussianAffineWeights(index.indices(1).size, initWeightsScale, rng)
//    }
//    // TODO: Could potentially flip these?
//    DenseVector.vertcat(DenseVector.zeros(index.indices(0).size),
//                        embeddingsInitialization,
//                        innerTransform.initialWeightVector(initWeightsScale, rng, false, spec))
//  }
//  
//  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) {
//    innerTransform.clipHiddenWeightVectors(weights(index.componentOffset(1) to -1), norm, false)
//  }
//
//  case class OutputLayer(weights: DenseMatrix[Double], embeddings: DenseMatrix[Double], bias: DenseVector[Double], innerLayer: innerTransform.Layer) extends OutputTransform.OutputLayer[FV,DenseVector[Double]] {
//    override val index = AffineOutputEmbeddingTransform.this.index
//
//    val weightst = weights.t
////    val weightst = weights.t.copy
//
//
//    def activations(fv: FV) = {
//      val innerActs = innerLayer.activations(fv)
//      DenseVector(Array.tabulate(numOutputs)(i => activationsFromPenultimateDot(innerActs, i)))
//    }
//    
//    def activationsFromPenultimateDot(innerLayerActivations: DenseVector[Double], sparseIdx: Int) = {
//      (weights * innerLayerActivations) dot embeddings(sparseIdx, ::).t + bias(sparseIdx)
//    }
//
//    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
//      val scale = _scale
//      val weightsDeriv = deriv(0 until (outputDim * numInputs)).asDenseMatrix.reshape(outputDim, numInputs, view = View.Require)
//      val embeddingsDeriv = deriv(index.componentOffset(1) until index.componentOffset(1) + (numOutputs * outputDim)).asDenseMatrix.reshape(numOutputs, outputDim, view = View.Require)
//      val biasDeriv = deriv(index.componentOffset(1) + numOutputs * outputDim until index.componentOffset(2))
//      val innerAct = innerLayer.activations(fv)
//      val innerScale = DenseVector(Array.tabulate(numInputs)(i => 0.0))
//      // TODO: Compute derivatives here
//      for (k <- 0 until scale.size) {
//        // Assuming there's something nontrivial to pass back
//        if (scale(k) != 0.0) {
//          // Bias update
//          biasDeriv(k) += scale(k)
//          // Dumb way
////          for (i <- 0 until numInputs) {
////            for (j <- 0 until outputDim) {
////              weightsDeriv(j, i) += scale(k) * innerAct(i) * embeddings(k, j)
////              embeddingsDeriv(k, j) += scale(k) * weights(j, i) * innerAct(i)
////              innerScale(i) += scale(k) * weights(j, i) * embeddings(k, j)
////            }
////          }
//          // Smart way; matches the dumb way exactly
//          // Column * row gives outer product
//          weightsDeriv += embeddings(k, ::).t * innerAct.t * scale(k) // Weights update
//          embeddingsDeriv(k, ::).t += weights * innerAct * scale(k) // Embeddings update
//          innerScale += weightst * embeddings(k, ::).t * scale(k) // Inner scale update
//        }
//      }
//      innerLayer.tallyDerivative(deriv(index.componentOffset(2) to -1), innerScale, fv)
//    }
//    
//    def applyBatchNormalization(inputs: scala.collection.GenTraversable[FV]) = innerLayer.applyBatchNormalization(inputs)
//  }
//
//}
//
//object AffineOutputEmbeddingTransform {
//  
//  def getIdentityEmbeddingWeights(numOutputs: Int, outputDim: Int, rng: Random) = {
//    require(outputDim <= numOutputs, outputDim + " " + numOutputs)
//    val mat = DenseMatrix.zeros[Double](numOutputs, outputDim)
//    for (i <- 0 until outputDim) {
//      mat(i, i) = 1.0
//    }
//    for (i <- outputDim until numOutputs) {
//      mat(i, rng.nextInt(outputDim)) = 1.0
//    }
//    val biasInitializer = DenseVector.zeros[Double](numOutputs)
//    val initWeights = DenseVector.vertcat(DenseVector(mat.data), biasInitializer)
//    initWeights
//  }
//  
//  def clipEmbeddingNorms(embeddings: DenseMatrix[Double]) {
//    for (i <- 0 until embeddings.rows) {
//      var norm = 0.0
//      for (j <- 0 until embeddings.cols) {
//        norm += embeddings(i, j) * embeddings(i, j)
//      }
//      norm = Math.sqrt(norm)
//      for (j <- 0 until embeddings.cols) {
//        embeddings(i, j) /= norm
//      }
//    }
//  }
//  
//  def displayEmbeddingNorms(embeddings: DenseMatrix[Double]) {
//    var avgNorm = 0.0
//    var maxNorm = 0.0
//    for (i <- 0 until embeddings.rows) {
//      var norm = 0.0
//      for (j <- 0 until embeddings.cols) {
//        norm += embeddings(i, j) * embeddings(i, j)
//      }
//      norm = Math.sqrt(norm)
//      avgNorm += norm
//      maxNorm = Math.max(maxNorm, norm)
//    }
//    println("Average norm: " + avgNorm/embeddings.rows + ", max norm: " + maxNorm)
//  }
//}