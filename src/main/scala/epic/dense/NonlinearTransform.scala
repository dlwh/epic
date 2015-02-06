package epic.dense

import breeze.linalg._
import breeze.linalg.DenseVector
import epic.framework.Feature
import breeze.util.Index
import scala.util.Random
import breeze.numerics.sigmoid

case class NonlinearTransform[FV](nonLinType: String, size: Int, inner: Transform[FV, DenseVector[Double]]) extends Transform[FV, DenseVector[Double]] {
  
  val index: inner.index.type = inner.index

  def extractLayer(dv: DenseVector[Double], forTrain: Boolean) = { 
    if (nonLinType == "dropout") {
      val keepFrac = 0.5  // 1 - dropout fraction
      val fcn = if (forTrain) {
        // Only have "true" when we want to keep things around
        new NonlinearTransform.Mask(Array.tabulate(size)(i => NonlinearTransform.globalRng.nextDouble < keepFrac)) 
      } else {
        new NonlinearTransform.Scale(keepFrac)
      }
      new Layer(fcn, inner.extractLayer(dv, forTrain))
    } else {
      val nonlinearFcn = NonlinearTransform.getNonlinearFcn(nonLinType);
      new Layer(nonlinearFcn, inner.extractLayer(dv, forTrain))
    }
  }
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = inner.initialWeightVector(initWeightsScale, rng, false, spec)

  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) = inner.clipHiddenWeightVectors(weights, norm, false)
  
  case class Layer(nonlinearFcn: NonlinearTransform.NonlinearFcn, innerLayer: inner.Layer) extends Transform.Layer[FV,DenseVector[Double]] {
    
    val myIndex = Index[Feature]
    
    def index = myIndex;

    def activations(fv: FV): DenseVector[Double] = {
      val act = innerLayer.activations(fv)
      var i = 0;
      while (i < act.size) {
        act(i) = nonlinearFcn.fcn(i, act(i))
        i += 1
      }
      act
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
      val scale = _scale
      val act = innerLayer.activations(fv)
      var i = 0;
      while (i < act.size) {
        act(i) = nonlinearFcn.deriv(i, act(i))
        i += 1
      }
      act :*= scale
      innerLayer.tallyDerivative(deriv, act, fv)
    }

  }

}

object NonlinearTransform {
  
  val globalRng = new scala.util.Random(0)
  
  def getNonlinearFcn(nonLinType: String) = {
    if (nonLinType == "tanh") {
      Tanh()
    } else if (nonLinType == "relu") {
      Relu()
    } else if (nonLinType == "cube") {
      Cube()
    } else {
      throw new RuntimeException("Unrecognized nonlin type: " + nonLinType)
    }
  }
  
  trait NonlinearFcn {
    // idx is the position of the unit; this basically only applies to dropout
    // where we want to zero out particular units
    def fcn(idx: Int, x: Double): Double;
    def deriv(idx: Int, x: Double): Double;
  }
  
  case class Mask(val mask: Array[Boolean]) extends NonlinearFcn {
    def fcn(idx: Int, x: Double) = if (mask(idx)) x else 0
    def deriv(idx: Int, x: Double) = if (mask(idx)) 1 else 0
  }
  
  case class Scale(val factor: Double) extends NonlinearFcn {
    def fcn(idx: Int, x: Double) = factor * x
    def deriv(idx: Int, x: Double) = factor
  }
  
  case class Tanh() extends NonlinearFcn {
    def fcn(idx: Int, x: Double) = 2 * sigmoid(2 * x) - 1.0
    def deriv(idx: Int, x: Double) = {
      val sig = sigmoid(2 * x)
      -4 * sig * (sig - 1.0)
    }
  }
  
  case class Relu() extends NonlinearFcn {
    def fcn(idx: Int, x: Double) = Math.max(x, 0)
    def deriv(idx: Int, x: Double) = if (x > 0) 1.0 else 0.0
  }
  
  case class Cube() extends NonlinearFcn {
    def fcn(idx: Int, x: Double) = x * x * x
    def deriv(idx: Int, x: Double) = 2 * x * x
  }
  
}