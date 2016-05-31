package epic.logo

import breeze.math.MutableInnerProductModule

case class LogLikelihoodDecoder[T, W, OracleS, ExpectationS](
    inferencer: OracleInferencer[T, W, OracleS], summer: ExpectationInferencer[T, W, ExpectationS])(
        implicit space: MutableInnerProductModule[W, Double]) extends Decoder[T, W, OracleS, ExpectationS] {
  import space._

  def decode(weights : Weights[W], instance : T) : (W, Double, OracleS, ExpectationS) = {
    val (y_*, f_*, l_*, state_*) = inferencer.oracle(weights, instance)
    val (f, l, state) = summer.expectations(weights, instance)
    ((f_* - f), l - l_*, state_*, state)
  }

  def initialState = (inferencer.initialState, summer.initialState)
  def reduceStates(states1: (OracleS, ExpectationS), states2: (OracleS, ExpectationS)): (OracleS, ExpectationS) =
    (inferencer.reduceStates(states1._1, states2._1),
      summer.reduceStates(states1._2, states2._2))
}
