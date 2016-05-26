package epic.logo

import breeze.math.MutableInnerProductModule

class MaxMarginRankingDecoder[T, W](val inferencer : LossAugmentedArgmaxInferencer[T, _, W], val gamma : Double = 0.0)(implicit space: MutableInnerProductModule[W, Double]) extends Decoder[T, W] {
import space._
  def decode(weights: Weights[W], instance: T): (W, Double) = {
    val (y_min, df_min, l_min) = inferencer.lossAugmentedArgmax(weights, instance, -1.0, -(1.0 + gamma))
    val (y_max, df_max, l_max) = inferencer.lossAugmentedArgmax(weights, instance, 1.0, 1.0)
    (df_min - df_max, l_max - (1.0 + gamma) * l_min)
  }

}
