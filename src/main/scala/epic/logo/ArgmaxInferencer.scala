package epic.logo

trait ArgmaxInferencer[T, Y, W] {

  def argmax(weights : Weights[W], instance : T) : (Y, FeatureVector[W], Double)

}
