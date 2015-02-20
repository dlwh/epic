package epic.corefdense

import breeze.linalg._
import breeze.linalg.support.{CanMapValues, CanZipMapValues, CanTraverseValues}
import breeze.numerics._
import breeze.stats.distributions.{Rand, RandBasis}
import breeze.optimize.StochasticDiffFunction
import breeze.optimize.StochasticGradientDescent

/**
 * Implements the L2^2 and L1 updates from
 * Duchi et al 2010 Adaptive Subgradient Methods for Online Learning and Stochastic Optimization.
 *
 * Basically, we use "forward regularization" and an adaptive step size based
 * on the previous gradients.
 *
 * @author dlwh
 */
class CombinationAdaptiveGradientDescentDVD(val regularizationConstant1: Double,
                                            stepSize1: Double,
                                            val regularizationConstant2: Double,
                                            stepSize2: Double,
                                            indexToSwitchAt: Int,
                                            maxIter: Int,
                                            tolerance: Double = 1E-5,
                                            improvementTolerance: Double = 1E-4,
                                            minImprovementWindow: Int = 50)
    extends StochasticGradientDescent[DenseVector[Double]](1.0, maxIter, tolerance, improvementTolerance, minImprovementWindow) {

    val delta = 1E-4

    case class History(sumOfSquaredGradients: DenseVector[Double])
    override def initialHistory(f: StochasticDiffFunction[DenseVector[Double]], init: DenseVector[Double]) = History(DenseVector.zeros[Double](init.size))

    override def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newValue: Double, f: StochasticDiffFunction[DenseVector[Double]], oldState: State) = {
      val oldHistory = oldState.history
      val newG = (oldState.grad :* oldState.grad)
      val maxAge = 1000.0
      if(oldState.iter > maxAge) {
        newG *= 1/maxAge
        axpy((maxAge - 1)/maxAge, oldHistory.sumOfSquaredGradients, newG)
      } else {
        newG += oldHistory.sumOfSquaredGradients
      }
      new History(newG)
    }

    override protected def takeStep(state: State, dir: DenseVector[Double], stepSize: Double) = {
      import state._
      val s = sqrt(state.history.sumOfSquaredGradients :+ (state.grad :* state.grad))
      val newx = x :* s
      // MODIFIED
      axpy(stepSize1, dir(0 until indexToSwitchAt), newx(0 until indexToSwitchAt))
      axpy(stepSize2, dir(indexToSwitchAt to -1), newx(indexToSwitchAt to -1))
      s(0 until indexToSwitchAt) += (delta + regularizationConstant1 * stepSize1)
      s(indexToSwitchAt to -1) += (delta + regularizationConstant2 * stepSize2)
      // END MODIFIED
      newx :/= s
      newx
    }

    override def determineStepSize(state: State, f: StochasticDiffFunction[DenseVector[Double]], dir: DenseVector[Double]) = {
      // MODIFIED
      // Return 1.0 and don't use this stepSize
      1.0
//      defaultStepSize
    }

    override protected def adjust(newX: DenseVector[Double], newGrad: DenseVector[Double], newVal: Double) = {
      // MODIFIED
      val av = newVal + (newX(0 until indexToSwitchAt) dot newX(0 until indexToSwitchAt)) * regularizationConstant1 / 2.0 +
                        (newX(indexToSwitchAt to -1) dot newX(indexToSwitchAt to -1)) * regularizationConstant2 / 2.0
      val ag = newGrad + DenseVector.vertcat(newX(0 until indexToSwitchAt) * regularizationConstant1, newX(indexToSwitchAt to -1) * regularizationConstant2)
      // END MODIFIED
      (av -> ag)
    }

  }