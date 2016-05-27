package epic.logo

import breeze.math.MutableInnerProductModule

case class LossAugmentedMaxMarginDecoder[T, W, OracleS, ArgmaxerS](
    oracleInferencer: OracleInferencer[T, W, OracleS],
    argmaxer: LossAugmentedArgmaxInferencer[T, W, ArgmaxerS])(implicit space: MutableInnerProductModule[W, Double])
    extends Decoder[T, W, OracleS, ArgmaxerS] {
  import space._

  def decode(weights: Weights[W], instance: T): (W, Double, OracleS, ArgmaxerS) = {
    val (y_*, f_*, l_*, state_*) = oracleInferencer.oracle(weights, instance)
    val (y, f, l, state) = argmaxer.lossAugmentedArgmax(weights, instance, 1.0, 1.0)
    (f_* - f, l - l_*, state_*, state)
  }

  def initialState: (OracleS, ArgmaxerS) = (oracleInferencer.initialState, argmaxer.initialState)
  def reduceStates(states1: (OracleS, ArgmaxerS), states2: (OracleS, ArgmaxerS)): (OracleS, ArgmaxerS) =
    (oracleInferencer.reduceStates(states1._1, states2._1),
      argmaxer.reduceStates(states1._2, states2._2))

}
