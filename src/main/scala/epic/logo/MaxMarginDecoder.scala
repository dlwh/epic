package epic.logo

import breeze.math.MutableInnerProductModule

class MaxMarginDecoder[T, W](val oracleInferencer : OracleInferencer[T, _, W], val argmaxer : ArgmaxInferencer[T, _, W])(implicit space: MutableInnerProductModule[W, Double]) extends Decoder[T, W] {
  import space._

  def decode(weights : Weights[W], instance : T) : (W, Double) = {
    val (y_*, f_*, l_*) = oracleInferencer.oracle(weights, instance)
    val (y, f, l) = argmaxer.argmax(weights, instance)
    (f_* - f, l - l_*)
  }

}
