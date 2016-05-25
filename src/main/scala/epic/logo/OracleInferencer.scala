package epic.logo

trait OracleInferencer[T, Y, W] {

  def oracle(weights : Weights[W], instance : T) : (Y, FeatureVector[W], Double)

}
