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
case class CachingLookupTransform(word2vecIndexed: Word2VecIndexed[String]) extends Transform[Array[Int], DenseVector[Double]] {

  val index = Index[epic.framework.Feature]()
  
  def extractLayer(weights: DenseVector[Double], forTrain: Boolean) = new Layer()
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = DenseVector()
  
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) {}
  
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int] = Seq[Int]()

  case class Layer() extends Transform.Layer[Array[Int],DenseVector[Double]] {
    
    override val index = Index[epic.framework.Feature]()

    def activations(fv: Array[Int]) = {
      var finalVector = DenseVector.zeros[Double](0)
      fv.indices.foreach { i =>
        val vec: DenseVector[Double] = if (fv(i) != -1) DenseVector(word2vecIndexed.convertIndexToVector(fv(i))) else DenseVector(word2vecIndexed.zeroVector)
        finalVector = DenseVector.vertcat(finalVector, vec)
      }
      finalVector
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: Array[Int]) = {}
    
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[Array[Int]]) = {}
  }
}