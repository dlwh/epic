package epic.dense

import breeze.linalg._
import breeze.util.Index
import epic.framework.Feature
import scala.util.Random

class IdentityTransform[T] extends Transform[T, T] {

  val index = Index[Feature]()

  def extractLayer(weights: DenseVector[Double], forTrain: Boolean) = new Layer()
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = DenseVector(Array[Double]())
  
  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) {}
  
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int] = Seq[Int]()

  class Layer extends Transform.Layer[T,T] {
    
    val myIndex = Index[Feature]
    
    def index = myIndex

    def activations(fv: T) = fv

    def tallyDerivative(deriv: DenseVector[Double], scale: =>Vector[Double], t: T) = {}
    
    def applyBatchNormalization(inputs: scala.collection.GenTraversable[T]) = {}
  }

}

