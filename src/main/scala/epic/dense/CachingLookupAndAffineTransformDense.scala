package epic.dense

import scala.runtime.ScalaRunTime
import breeze.linalg._
import epic.features.SegmentedIndex
import epic.framework.Feature
import scala.collection.mutable.HashMap
import scala.util.Random

/**
 * Used at the input layer to cache lookups and 
 */
case class CachingLookupAndAffineTransformDense[FV](numOutputs: Int,
                                                    numInputs: Int,
                                                    word2vecIndexed: Word2VecIndexed[String],
                                                    includeBias: Boolean = true) extends Transform[Array[Int], DenseVector[Double]] {


  val index = new AffineTransform.Index(numOutputs, numInputs, includeBias)
  
  def extractLayer(weights: DenseVector[Double], forTrain: Boolean) = {
    val mat = weights(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
    val bias = if(includeBias) {
      weights(numOutputs * numInputs until index.size)
    } else {
      DenseVector.zeros[Double](numOutputs)
    }
    new Layer(mat, bias)
  }
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = {
    val myWeights = if (outputLayer) {
      DenseVector(Array.tabulate(index.size)(i => 0.0))
    } else if (spec == "magic") {
      AffineTransform.getMagicAffineWeights(index.size, numInputs, numOutputs, initWeightsScale, rng)
    } else {
      AffineTransform.getGaussianAffineWeights(index.size, initWeightsScale, rng)
    }
    myWeights
//    DenseVector(Array.tabulate(index.size)(i => if (!outputLayer) rng.nextGaussian * initWeightsScale else 0.0))
  }
  
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) {
    if (!outputLayer) {
      AffineTransform.clipHiddenWeightVectors(numOutputs, numInputs, weights, norm)
    }
  }
  
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int] = {
    (offset until offset + Math.min(10, index.size))
  }

  case class Layer(weights: DenseMatrix[Double], bias: DenseVector[Double]) extends Transform.Layer[Array[Int],DenseVector[Double]] {
    
    override val index = CachingLookupAndAffineTransformDense.this.index
    
    val weightst = weights.t

    // Cache stores pairs of (word identity, position) mapped to the final results of
    // these being multiplied by the parameter vector. Note that although the same
    // word vector is used for each word identity, the parameter vector depends
    // on the position.
//    val cache = new HashMap[(Int,Int),DenseVector[Double]]
    val caches = Array.tabulate(numInputs/word2vecIndexed.wordRepSize)(i => new HashMap[Int,DenseVector[Double]])

    def activations(fv: Array[Int]) = {
      val finalVector = DenseVector.zeros[Double](numOutputs)
      for (i <- 0 until fv.size) {
//        val wordPosn = fv(i) -> i
        if (fv(i) != -1) {
          caches(i).synchronized {
            if (!caches(i).contains(fv(i))) {
              val startIdx = i * word2vecIndexed.wordRepSize
              caches(i).put(fv(i), weights(::, startIdx until startIdx + word2vecIndexed.wordRepSize) * DenseVector(word2vecIndexed.convertIndexToVector(fv(i))))
            }
            finalVector += caches(i)(fv(i))
          }
//          cache.synchronized {
//            if (!cache.contains(wordPosn)) {
//              val startIdx = i * word2vecIndexed.wordRepSize
//              cache.put(wordPosn, weights(::, startIdx until startIdx + word2vecIndexed.wordRepSize) * DenseVector(word2vecIndexed.word2vec(wordPosn._1)))
//            }
//            finalVector += cache(wordPosn)
//          }
//          val startIdx = i * word2vecFeaturizer.wordRepSize
//          finalVector += weights(::, startIdx until startIdx + word2vecFeaturizer.wordRepSize) * DenseVector(word2vecFeaturizer.word2vec(wordPosn._1))
        }
      }
      finalVector + bias
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: Array[Int]) = {
      val scale = _scale
      val matDeriv = deriv(0 until (numOutputs * numInputs)).asDenseMatrix.reshape(numOutputs, numInputs, view = View.Require)
      val biasDeriv = if(includeBias) {
        deriv(numOutputs * numInputs until index.size)
      } else {
        DenseVector.zeros[Double](numOutputs)
      }

      // whole function is f(mat * inner(fv) + bias)
      // scale(i) pushes in  (f'(mat * inner(v) + bias))(i)
      val innerAct = DenseVector(word2vecIndexed.convertToVector(fv));
      
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
    }
    
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[Array[Int]]) = {}
  }
}
