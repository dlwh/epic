package epic.logo

trait LossAugmentedExpectationInferencer[T, W,  S] extends ExpectationInferencer[T, W, S] {

  def expectations(weights : Weights[W], instance : T) : (W, Double, S) = lossAugmentedExpectations(weights, instance, 1.0, 0.0)

  def lossAugmentedExpectations(weights : Weights[W], instance : T, weightsWeight : Double, lossWeight : Double) : (W, Double, S)

}
