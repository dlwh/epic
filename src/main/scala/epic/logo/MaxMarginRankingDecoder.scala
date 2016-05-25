package epic.logo

class MaxMarginRankingDecoder[T, W](val inferencer : LossAugmentedArgmaxInferencer[T, _, W], val gamma : Double = 0.0) extends Decoder[T, W] {

  def decode(weights : Weights[W], instance : T) : (FeatureVector[W], Double) = {
    val (y_min, df_min, l_min) = inferencer.lossAugmentedArgmax(weights, instance, -1.0, -(1.0 + gamma))
    val (y_max, df_max, l_max) = inferencer.lossAugmentedArgmax(weights, instance, 1.0, 1.0)
    (df_min - df_max, l_max - (1.0 + gamma) * l_min)
  }

}
