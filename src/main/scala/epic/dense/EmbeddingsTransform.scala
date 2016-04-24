package epic.dense

import scala.runtime.ScalaRunTime
import breeze.linalg._
import epic.features.SegmentedIndex
import epic.framework.Feature
import scala.collection.mutable.HashMap
import scala.util.Random

/**
 * Used at the input layer to cache lookups and
 * backprop into embeddings 
 */
case class EmbeddingsTransform[FV](numOutputs: Int,
                                   numInputs: Int,
                                   word2vecIndexed: Word2VecIndexed[String],
                                   includeBias: Boolean = true) extends Transform[Array[Int], DenseVector[Double]] {


  val index = SegmentedIndex(new AffineTransform.Index(numOutputs, numInputs, includeBias),
                             new AffineTransform.Index(word2vecIndexed.vocSize, word2vecIndexed.wordRepSize, false))
  println("Allocated " + index.indices.map(_.size) + " parameters for each index in the embedding layer (backpropagating into embeddings)")
  
  def extractLayer(weights: DenseVector[Double], forTrain: Boolean) = {
    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
    val bias = if (includeBias) {
      weights(numOutputs * numInputs until index.indices(0).size)
    } else {
      DenseVector.zeros[Double](numOutputs)
    }
    val wordWeights = weights(index.indices(0).size until index.indices(0).size + index.indices(1).size).asDenseMatrix.reshape(word2vecIndexed.vocSize, word2vecIndexed.wordRepSize, view = View.Require)
    new Layer(mat, bias, wordWeights)
  }
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = {
    val myWeights = if (outputLayer) {
      DenseVector(Array.tabulate(index.indices(0).size)(i => 0.0))
    } else if (spec == "magic") {
      AffineTransform.getMagicAffineWeights(index.indices(0).size, numInputs, numOutputs, initWeightsScale, rng)
    } else {
      AffineTransform.getGaussianAffineWeights(index.indices(0).size, initWeightsScale, rng)
    }
    // Only randomly initialize the weights in the matrix, not the word deltas
    DenseVector.vertcat(myWeights, DenseVector.zeros[Double](index.size - index.indices(0).size))
//    DenseVector(Array.tabulate(index.size)(i => if (!outputLayer && i < index.indices(0).size) rng.nextGaussian * initWeightsScale else 0.0))
  }
  
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) {
    if (!outputLayer) {
      AffineTransform.clipHiddenWeightVectors(numOutputs, numInputs, weights, norm)
    }
  }
  
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int] = {
    (offset until offset + Math.min(10, index.indices(0).size)) ++ (offset + index.componentOffset(1) until offset + index.componentOffset(1) + Math.min(10, index.indices(1).size))
  }

  case class Layer(weights: DenseMatrix[Double], bias: DenseVector[Double], wordWeights: DenseMatrix[Double]) extends Transform.Layer[Array[Int],DenseVector[Double]] {
    
    override val index = EmbeddingsTransform.this.index
    
    val weightst = weights.t

    // Cache stores pairs of (word identity, position) mapped to the final results of
    // these being multiplied by the parameter vector. Note that although the same
    // word vector is used for each word identity, the parameter vector depends
    // on the position.
    val caches = Array.tabulate(numInputs/word2vecIndexed.wordRepSize)(i => new HashMap[Int,DenseVector[Double]])

    def activations(fv: Array[Int]) = {
      val finalVector = DenseVector.zeros[Double](numOutputs)
      fv.indices.foreach { i =>
      // val wordPosn = fv(i) -> i
        if (fv(i) != -1) {
          caches(i).synchronized {
            if (!caches(i).contains(fv(i))) {
              val startIdx = i * word2vecIndexed.wordRepSize
              val wordVec = DenseVector(word2vecIndexed.convertIndexToVector(fv(i))) + wordWeights(fv(i), ::).t
              caches(i).put(fv(i), weights(::, startIdx until startIdx + word2vecIndexed.wordRepSize) * wordVec)
            }
            finalVector += caches(i)(fv(i))
          }
        }
      }
      finalVector + bias
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: Array[Int]) = {
      val scale = _scale
      val matDeriv = deriv(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
      val biasDeriv = if (includeBias) {
        deriv(numOutputs * numInputs until index.size)
      } else {
        DenseVector.zeros[Double](numOutputs)
      }
      // whole function is f(mat * inner(fv) + bias)
      // scale(i) pushes in  (f'(mat * inner(v) + bias))(i)
      val innerAct = DenseVector(word2vecIndexed.convertToVector(fv)) + Word2VecSurfaceFeaturizerIndexed.makeVectFromParams(fv, wordWeights)
      val wordsDeriv = deriv(index.indices(0).size until index.indices(0).size + index.indices(1).size).asDenseMatrix.reshape(word2vecIndexed.vocSize, word2vecIndexed.wordRepSize, view = View.Require)
      val wordsDerivs = Array.tabulate(fv.length)(wordPosnIdx => wordsDeriv(fv(wordPosnIdx), ::).t)
      // d/d(weights(::, i)) == scale(i) * innerAct
      for (i <- 0 until weights.rows) {
        val a: Double = scale(i)
        if (a != 0.0) {
          axpy(a, innerAct, matDeriv.t(::, i))
          var wordPosnIdx = 0
          while (wordPosnIdx < fv.length) {
            val relevantWeights = weights(i, wordPosnIdx * word2vecIndexed.wordRepSize until (wordPosnIdx + 1) * word2vecIndexed.wordRepSize).t
            axpy(a, relevantWeights, wordsDerivs(wordPosnIdx))
            wordPosnIdx += 1
          }
        // so d/dbias(i) = scale(i)
          biasDeriv(i) += a
        }
      }

      // scale is f'(mat * inner(v) + bias)
      // d/dv is mat.t * f'(mat * inner(v) + bias)
    }

    def applyBatchNormalization(inputs: scala.collection.GenTraversable[Array[Int]]) = {}
  }
}