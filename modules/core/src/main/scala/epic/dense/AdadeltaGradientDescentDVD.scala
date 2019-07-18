package epic.dense

import breeze.linalg._
import breeze.numerics._
import breeze.optimize.StochasticDiffFunction
import breeze.optimize.StochasticGradientDescent


class AdadeltaGradientDescentDVD(maxIter: Int,
                              rho: Double = 0.95,
                              tolerance: Double = 1E-5,
                              minImprovementWindow: Int = 50)
    extends StochasticGradientDescent[DenseVector[Double]](1.0, maxIter, tolerance, minImprovementWindow) {

  val delta = 1E-4
  val epsilon = 1e-6
  import vspace._
  
  case class History(squaredGradientsHistory: DenseVector[Double], squaredUpdatesHistory: DenseVector[Double])
  override def initialHistory(f: StochasticDiffFunction[DenseVector[Double]],init: DenseVector[Double]) = {
    History(DenseVector(Array.tabulate(init.size)(i => 1e-6)), DenseVector(Array.tabulate(init.size)(i => 1e-6)))
  }

  override def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newValue: Double, f: StochasticDiffFunction[DenseVector[Double]], oldState: State) = {
    val oldHistory = oldState.history
    // The new gradient gets incorporated during the next round of takeStep,
    // so this computation should lag by one
    val newG = (oldState.grad :* oldState.grad) * (1 - rho)
    axpy(rho, oldHistory.squaredGradientsHistory, newG)
    val deltaX = newX - oldState.x
    val newU = deltaX :* deltaX * (1 - rho)
    axpy(rho, oldHistory.squaredUpdatesHistory, newU)
    new History(newG, newU)
  }

  override protected def takeStep(state: State, dir: DenseVector[Double], stepSize: Double) = {
    import state._
    // Need to pre-emptively update the gradient since the history only has it through the
    // last timestep
    val rmsGt = sqrt((state.history.squaredGradientsHistory * rho) :+ ((state.grad :* state.grad) * (1-rho)) :+ epsilon)
    val rmsDeltaXtm1 = sqrt(state.history.squaredUpdatesHistory :+ epsilon)
    val step = dir :* rmsDeltaXtm1 :/ rmsGt
    val newX = x
    axpy(1.0, step, newX)
    newX
  }

  override def determineStepSize(state: State, f: StochasticDiffFunction[DenseVector[Double]], dir: DenseVector[Double]) = {
    defaultStepSize // pegged to 1.0 for this method
  }

  override protected def adjust(newX: DenseVector[Double], newGrad: DenseVector[Double], newVal: Double) = {
    newVal -> newGrad
  }

}