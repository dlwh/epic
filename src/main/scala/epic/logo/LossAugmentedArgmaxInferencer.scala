package epic.logo

trait LossAugmentedArgmaxInferencer[T, Y, W] extends ArgmaxInferencer[T, Y, W] {

  def argmax(weights : Weights[W], instance : T) : (Y, FeatureVector[W], Double) = lossAugmentedArgmax(weights, instance, 1.0, 0.0)

  def lossAugmentedArgmax(weights : Weights[W], instance : T, weightsWeight : Double, lossWeight : Double) : (Y, FeatureVector[W], Double)

}
