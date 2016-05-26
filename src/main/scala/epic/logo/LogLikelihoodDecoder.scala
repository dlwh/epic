package epic.logo

import breeze.math.MutableInnerProductModule

class LogLikelihoodDecoder[T, W](
    val inferencer: OracleInferencer[T, _, W], val summer: ExpectationInferencer[T, W])(
        implicit space: MutableInnerProductModule[W, Double]) extends Decoder[T, W] {
  import space._

  def decode(weights : Weights[W], instance : T) : (W, Double) = {
    val (y_*, f_*, l_*) = inferencer.oracle(weights, instance)
    val (f, l) = summer.expectations(weights, instance)
    ((f_* - f), l - l_*)
  }

}
