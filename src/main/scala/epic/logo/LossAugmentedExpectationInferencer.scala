package epic.logo

trait LossAugmentedExpectationInferencer[T, W] extends ExpectationInferencer[T, W] {

  def expectations(weights : Weights[W], instance : T) : (FeatureVector[W], Double) = lossAugmentedExpectations(weights, instance, 1.0, 0.0)

  def lossAugmentedExpectations(weights : Weights[W], instance : T, weightsWeight : Double, lossWeight : Double) : (FeatureVector[W], Double)

}
