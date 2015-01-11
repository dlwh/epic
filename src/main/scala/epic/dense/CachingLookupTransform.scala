package epic.dense

import scala.runtime.ScalaRunTime
import breeze.linalg._
import epic.features.SegmentedIndex
import epic.framework.Feature
import scala.collection.mutable.HashMap
import scala.util.Random
import breeze.util.Index

/**
 * Used at the input layer to cache lookups and 
 */
case class CachingLookupTransform(word2vecFeaturizer: Word2VecSurfaceFeaturizerIndexed[String]) extends Transform[Array[Int], DenseVector[Double]] {


  val index = Index[epic.framework.Feature]()
  
  def extractLayer(weights: DenseVector[Double]) = {
    new Layer()
  }
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean) = {
    DenseVector()
  }

  case class Layer() extends Transform.Layer[Array[Int],DenseVector[Double]] {
    
    override val index = Index[epic.framework.Feature]()

    def activations(fv: Array[Int]) = {
      var finalVector = DenseVector.zeros[Double](0)
      for (i <- 0 until fv.size) {
        val vec: DenseVector[Double] = if (fv(i) != -1) DenseVector(word2vecFeaturizer.word2vec(fv(i))) else DenseVector(word2vecFeaturizer.zeroVector)
        finalVector = DenseVector.vertcat(finalVector, vec)
      }
      finalVector
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: Array[Int]) = {
    }
  }
}