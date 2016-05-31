package epic.logo

trait OracleInferencer[T, W, S] extends Inferencer[S] {
  type Y

  def oracle(weights : Weights[W], instance : T) : (Y, W, Double, S)
}
