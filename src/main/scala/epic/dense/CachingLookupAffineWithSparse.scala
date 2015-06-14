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
case class CachingLookupAffineWithSparse[FV](numOutputs: Int,
                                             numWordInputs: Int,
                                             word2vecIndexed: Word2VecIndexed[String],
                                             auxInputDomainSizes: Array[Int],
                                             includeBias: Boolean = true) extends Transform[Array[Int], DenseVector[Double]] {
 
  val numAuxInputs = auxInputDomainSizes.size
  val totalAuxInputEntries = auxInputDomainSizes.foldLeft(0)(_ + _)
  val numInputs = numWordInputs * word2vecIndexed.wordRepSize + totalAuxInputEntries
  
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
    
    override val index = CachingLookupAffineWithSparse.this.index
    
    val weightst = weights.t

    // Cache stores pairs of (word identity, position) mapped to the final results of
    // these being multiplied by the parameter vector. Note that although the same
    // word vector is used for each word identity, the parameter vector depends
    // on the position.
//    val cache = new HashMap[(Int,Int),DenseVector[Double]]
    val caches = Array.tabulate(numWordInputs)(i => new HashMap[Int,DenseVector[Double]])

    def activations(fv: Array[Int]) = {
      val finalVector = DenseVector.zeros[Double](numOutputs)
      for (i <- 0 until numWordInputs) {
//        val wordPosn = fv(i) -> i
        if (fv(i) != -1) {
          caches(i).synchronized {
            if (!caches(i).contains(fv(i))) {
              val startIdx = i * word2vecIndexed.wordRepSize
              caches(i).put(fv(i), weights(::, startIdx until startIdx + word2vecIndexed.wordRepSize) * DenseVector(word2vecIndexed.convertIndexToVector(fv(i))))
            }
            finalVector += caches(i)(fv(i))
          }
        }
      }
      var totalOffset = numWordInputs * word2vecIndexed.wordRepSize;
      for (i <- 0 until auxInputDomainSizes.size) {
        // Add the weights corresponding to the active indicator feature for this one (assumes one
        // is always on)
        finalVector += weights(::, totalOffset + fv(numWordInputs + i))
        totalOffset += auxInputDomainSizes(i)
      }      
      finalVector + bias
    }

    def densifySparseVector(fv: Array[Int]) = {
      val finalArr = Array.tabulate(totalAuxInputEntries)(i => 0.0)
      var offsetIdx = 0
      for (i <- 0 until numAuxInputs) {
        finalArr(offsetIdx + fv(i)) = 1.0
        offsetIdx += auxInputDomainSizes(i)
      }
      DenseVector(finalArr)
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
      val innerAct = DenseVector.vertcat(DenseVector(word2vecIndexed.convertToVector(fv.slice(0, numWordInputs))),
                                         densifySparseVector(fv.slice(numWordInputs, fv.size)));
      
      // d/d(weights(::, i)) == scale(i) * innerAct
      for (i <- 0 until weights.rows) {
        val a: Double = scale(i)
        if(a != 0.0) {
          axpy(a, innerAct, matDeriv.t(::, i))
        // so d/dbias(i) = scale(i)
          biasDeriv(i) += a
        }
      }
    }
    
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[Array[Int]]) = {}
  }
}
