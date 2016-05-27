package epic.logo

trait LossAugmentedArgmaxInferencer[T, W, S] extends ArgmaxInferencer[T, W, S] {

  def argmax(weights : Weights[W], instance : T) : (Y, W, Double, S) = lossAugmentedArgmax(weights, instance, 1.0, 0.0)

  def lossAugmentedArgmax(weights : Weights[W], instance : T, weightsWeight : Double, lossWeight : Double) : (Y, W, Double, S)

}
