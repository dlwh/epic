package epic.logo

trait ExpectationInferencer[T, W] {

  def expectations(weights : Weights[W], instance : T) : (W, Double)

}
