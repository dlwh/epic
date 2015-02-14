package epic.corefdense

import breeze.linalg._
import breeze.linalg.support.{CanMapValues, CanZipMapValues, CanTraverseValues}
import breeze.numerics._
import breeze.stats.distributions.{Rand, RandBasis}
import breeze.optimize.StochasticDiffFunction
import breeze.optimize.StochasticGradientDescent


class StochasticGradientDescentMomentumDVD(maxIter: Int,
                                           sgdStepSize: Double = 0.01,
                                           momentum: Double = 0.9,
                                           regularization: Double = 0,
                                           tolerance: Double = 1E-5,
                                           improvementTolerance: Double = 1E-4,
                                           minImprovementWindow: Int = 50)
    extends StochasticGradientDescent[DenseVector[Double]](sgdStepSize, maxIter, tolerance, improvementTolerance, minImprovementWindow) {

  import vspace._
  
  case class History(pastStep: DenseVector[Double])
  override def initialHistory(f: StochasticDiffFunction[DenseVector[Double]],init: DenseVector[Double]) = {
    History(DenseVector(Array.tabulate(init.size)(i => 0.0)))
  }

  override def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newValue: Double, f: StochasticDiffFunction[DenseVector[Double]], oldState: State) = {
    new History(newX - oldState.x)
  }

  override protected def takeStep(state: State, dir: DenseVector[Double], stepSize: Double) = {
//    println(state.history.pastStep.norm() + " " + momentum)
    val step = state.history.pastStep * momentum + dir * stepSize
//    println(step.norm() + " " + dir.norm() + " " + stepSize)
    val newX = state.x + step
//    val newX = state.x
//    axpy(1.0, step, newX)
//    println((newX - state.x).norm())
    newX
  }

  override def determineStepSize(state: State, f: StochasticDiffFunction[DenseVector[Double]], dir: DenseVector[Double]) = {
    sgdStepSize
  }

  override protected def adjust(newX: DenseVector[Double], newGrad: DenseVector[Double], newVal: Double) = {
    if (regularization == 0) {
      newVal -> newGrad
    } else {
      val av = newVal + (newX dot newX) * regularization
      val ag = newGrad + newX * regularization
      (av -> ag)
    }
  }

}