package epic.logo

trait ExpectationInferencer[T, W, S] extends Inferencer[S] {

  def expectations(weights: Weights[W], instance: T): (W, Double, S)
}
