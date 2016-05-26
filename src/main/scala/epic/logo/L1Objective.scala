package epic.logo

import breeze.linalg._
import breeze.math.MutableInnerProductModule

object L1Objective {
  def slack[W](i : Instance[_, W], w : Weights[W]) = {
    if (i.constraints.isEmpty)
      Double.NegativeInfinity
    else
      i.constraints.map { case (df, l) => l - df * w }.max
  }

}

class L1Objective[W](val C : Double)(implicit space: MutableInnerProductModule[W, Double]) extends ObjectiveFunction[W] {

  def calculatePrimalAndDual(w : Weights[W], data : Seq[Instance[_, W]]): (Double, Double) = {
    w.checkNorm
    val calc_w = new Weights(space.zeroLike(w.array))
    data.flatMap(instance => instance.alphas zip instance.constraints).map { case (alpha, (df, l)) => df * alpha }.foreach(fv => {
      calc_w += fv
    })
    assert(data.forall(instance => NumUtils.approxEquals(C, instance.alphas.sum, 1e-5)))
    assert(w.approxEquals(calc_w))
    val primal = {
      val slackSum = data.map(i => L1Objective.slack(i, w)).sum
      0.5 * (w.`^2`) + C * slackSum
    }
    val dual = {
      val lossSum = data.flatMap(instance => instance.alphas zip instance.constraints).map { case (alpha, (df, l)) => alpha * l }.sum
      -0.5 * (w.`^2`) + lossSum
    }
    (primal, dual)
  }

}
