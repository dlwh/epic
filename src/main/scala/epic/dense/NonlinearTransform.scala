package epic.dense

import breeze.linalg._
import breeze.linalg.DenseVector
import epic.framework.Feature
import breeze.util.Index
import scala.util.Random
import breeze.numerics.sigmoid

case class NonlinearTransform[FV](nonLinType: String, inner: Transform[FV, DenseVector[Double]]) extends Transform[FV, DenseVector[Double]] {
  
  val nonlinearFcn = NonlinearTransform.getNonlinearFcn(nonLinType);

  val index: inner.index.type = inner.index

  def extractLayer(dv: DenseVector[Double]) = new Layer(inner.extractLayer(dv))
  
  def initialWeightVector(initWeightsScale: Double, rng: Random, outputLayer: Boolean, spec: String) = inner.initialWeightVector(initWeightsScale, rng, false, spec)

  def clipHiddenWeightVectors(weights: DenseVector[Double], norm: Double, outputLayer: Boolean) = inner.clipHiddenWeightVectors(weights, norm, false)
  
  case class Layer(innerLayer: inner.Layer) extends Transform.Layer[FV,DenseVector[Double]] {
    
    val myIndex = Index[Feature]
    
    def index = myIndex;

    def activations(fv: FV): DenseVector[Double] = {
      val act = innerLayer.activations(fv)
      var i = 0;
      while (i < act.size) {
        act(i) = nonlinearFcn.fcn(act(i))
        i += 1
      }
      act
    }

    def tallyDerivative(deriv: DenseVector[Double], _scale: =>Vector[Double], fv: FV) = {
      val scale = _scale
      val act = innerLayer.activations(fv)
      var i = 0;
      while (i < act.size) {
        act(i) = nonlinearFcn.deriv(act(i))
        i += 1
      }
      act :*= scale
      innerLayer.tallyDerivative(deriv, act, fv)
    }

  }

}

object NonlinearTransform {
  
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
    def fcn(x: Double): Double;
    def deriv(x: Double): Double;
  }
  
  case class Tanh() extends NonlinearFcn {
    def fcn(x: Double) = 2 * sigmoid(2 * x) - 1.0
    def deriv(x: Double) = {
      val sig = sigmoid(2 * x)
      -4 * sig * (sig - 1.0)
    }
  }
  
  case class Relu() extends NonlinearFcn {
    def fcn(x: Double) = Math.max(x, 0)
    def deriv(x: Double) = if (x > 0) 1.0 else 0.0
  }
  
  case class Cube() extends NonlinearFcn {
    def fcn(x: Double) = x * x * x
    def deriv(x: Double) = 2 * x * x
  }
  
}