package epic.logo

import breeze.math.MutableInnerProductModule

case class MaxMarginDecoder[T, W, OracleS, MaxerS](
  oracleInferencer: OracleInferencer[T, W, OracleS],
  argmaxer: ArgmaxInferencer[T, W, MaxerS])(implicit space: MutableInnerProductModule[W, Double])
    extends Decoder[T, W, OracleS, MaxerS] {
  import space._

  def decode(weights : Weights[W], instance : T) : (W, Double, OracleS, MaxerS) = {
    val (y_*, f_*, l_*, state_*) = oracleInferencer.oracle(weights, instance)
    val (y, f, l, state) = argmaxer.argmax(weights, instance)
    (f_* - f, l - l_*, state_*, state)
  }

  def initialState: (OracleS, MaxerS) = (oracleInferencer.initialState, argmaxer.initialState)
  def reduceStates(states1: (OracleS, MaxerS), states2: (OracleS, MaxerS)): (OracleS, MaxerS) =
    (oracleInferencer.reduceStates(states1._1, states2._1),
      argmaxer.reduceStates(states1._2, states2._2))

}
