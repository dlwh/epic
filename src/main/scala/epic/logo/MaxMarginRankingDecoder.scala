package epic.logo

import breeze.math.MutableInnerProductModule

case class MaxMarginRankingDecoder[T, W, S](
    inferencer: LossAugmentedArgmaxInferencer[T, W, S],
    gamma: Double = 0.0)(implicit space: MutableInnerProductModule[W, Double]) extends Decoder[T, W, S, S] {
  import space._
  def decode(weights: Weights[W], instance: T): (W, Double, S, S) = {
    val (y_min, df_min, l_min, s_min) = inferencer.lossAugmentedArgmax(weights, instance, -1.0, -(1.0 + gamma))
    val (y_max, df_max, l_max, s_max) = inferencer.lossAugmentedArgmax(weights, instance, 1.0, 1.0)
    (df_min - df_max, l_max - (1.0 + gamma) * l_min, s_min, s_max)
  }

  def initialState: (S, S) = (inferencer.initialState, inferencer.initialState)
  def reduceStates(states1: (S, S), states2: (S, S)): (S, S) =
    (inferencer.reduceStates(states1._1, states2._1),
      inferencer.reduceStates(states1._2, states2._2))

}
