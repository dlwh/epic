package epic.logo

class LogLikelihoodDecoder[T, W](val inferencer : OracleInferencer[T, _, W], val summer : ExpectationInferencer[T, W]) extends Decoder[T, W] {

  def decode(weights : Weights[W], instance : T) : (FeatureVector[W], Double) = {
    val (y_*, f_*, l_*) = inferencer.oracle(weights, instance)
    val (f, l) = summer.expectations(weights, instance)
    ((f_* - f), l - l_*)
  }

}
