package epic.logo

class MaxMarginDecoder[T, W](val oracleInferencer : OracleInferencer[T, _, W], val argmaxer : ArgmaxInferencer[T, _, W]) extends Decoder[T, W] {

  def decode(weights : Weights[W], instance : T) : (FeatureVector[W], Double) = {
    val (y_*, f_*, l_*) = oracleInferencer.oracle(weights, instance)
    val (y, f, l) = argmaxer.argmax(weights, instance)
    (f_* - f, l - l_*)
  }

}
