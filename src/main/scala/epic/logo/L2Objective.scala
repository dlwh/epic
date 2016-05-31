package epic.logo

import breeze.math.MutableInnerProductModule

class L2Objective[W](val C: Double)(implicit space: MutableInnerProductModule[W, Double])
    extends ObjectiveFunction[W] {

  def calculatePrimalAndDual(w: Weights[W], data: Seq[DualVariableHolder[_, W]]): (Double, Double) = {
    val primal = {
      val slackSum = data.map(instance => instance.slack * instance.slack).sum
      0.5 * (w.`^2`) + C * slackSum
    }
    val dual = {
      val lossSum = data.flatMap(instance => instance.alphas zip instance.constraints).map {
        case (alpha, (df, l)) => alpha * l
      }.sum
      C * lossSum - primal
    }
    (primal, dual)
  }



}
