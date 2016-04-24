package epic.dense

import breeze.linalg._
import breeze.linalg.DenseVector
import epic.framework.Feature
import breeze.util.Index
import scala.util.Random
import breeze.numerics.sigmoid
import epic.features.SegmentedIndex

/**
 * Implements batch normalization from
 * http://arxiv.org/pdf/1502.03167v3.pdf
 * Basically, each unit is shifted and rescaled per minibatch so that its activations 
 * have mean 0 and variance 1. This has been demonstrated to help training deep networks,
 * but doesn't seem to help here.
 */
case class BatchNormalizationTransform[FV](size: Int, useBias: Boolean, inner: Transform[FV, DenseVector[Double]]) extends Transform[FV, DenseVector[Double]] {
  
  val index = if (useBias) {
    SegmentedIndex(new AffineTransform.Index(size, 0, true), inner.index)
  } else {
    inner.index
  }

  def extractLayer(dv: DenseVector[Double], forTrain: Boolean) = {
    if (useBias) {
      new Layer(dv(0 until size), size, inner.extractLayer(dv(size to -1), forTrain))
    } else {
      new Layer(DenseVector.zeros[Double](size), size, inner.extractLayer(dv, forTrain))
    }
  }
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = {
    if (useBias) {
      DenseVector.vertcat(DenseVector.zeros[Double](size),
                          inner.initialWeightVector(initWeightsScale, rng, false, spec))
    } else {
      inner.initialWeightVector(initWeightsScale, rng, false, spec)
    }
  }

  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) = inner.clipHiddenWeightVectors(weights, norm, false)
  
  def getInterestingWeightIndicesForGradientCheck(offset: Int): Seq[Int] = {
    if (useBias) {
      (offset until offset + Math.min(10, size)) ++ inner.getInterestingWeightIndicesForGradientCheck(offset + size)
    } else {
      inner.getInterestingWeightIndicesForGradientCheck(offset)
    }
  }
  
  case class Layer(bias: DenseVector[Double], size: Int, innerLayer: inner.Layer) extends Transform.Layer[FV,DenseVector[Double]] {
    
    var fcn = new NonlinearTransform.ShiftAndScaleEach(Array.tabulate(size)(i => 0.0), Array.tabulate(size)(i => 1.0))
    
    val myIndex = Index[Feature]
    
    def index = myIndex
    
    def activations(fv: FV): DenseVector[Double] = {
      val act = innerLayer.activations(fv)
      var i = 0
      while (i < act.size) {
        act(i) = fcn.fcn(i, act(i)) + bias(i)
        i += 1
      }
      act
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
      val biasDeriv = if (useBias) deriv(0 until size) else DenseVector[Double]()
      val scale = _scale
      var i = 0
      while (i < scale.size) {
        if (useBias) {
          biasDeriv(i) += scale(i)
        }
        scale(i) = scale(i) * fcn.deriv(i, 0) // we know it's linear so just evaluate the derivative at 0, saves computing activations
        i += 1
      }
      innerLayer.tallyDerivative(if (useBias) deriv(size to -1) else deriv, scale, fv)
    }

    def applyBatchNormalization(inputs: scala.collection.GenTraversable[FV]) = {
      val allActivations = inputs.map(activations(_))
      val mean = allActivations.reduce(_ + _) * (1.0/inputs.size)
      val variances = allActivations.map(act => (act - mean) :* (act - mean)).reduce(_ + _) * (1.0/inputs.size)
      val invStdDevs = variances.data.map(variance => 1.0/Math.sqrt(variance + 1e-6))
      // println(mean.data.toSeq)
      // println(invStdDevs.toSeq)
      fcn = new NonlinearTransform.ShiftAndScaleEach(mean.data, invStdDevs)
      innerLayer.applyBatchNormalization(inputs)
    }
  }

}