package epic.logo

trait Decoder[T, W, OracleS, MaxerS] {

  def decode(weights : Weights[W], instance : T) : (W, Double, OracleS, MaxerS)
  def initialState: (OracleS, MaxerS)
  def reduceStates(states1: (OracleS, MaxerS), states2: (OracleS, MaxerS)): (OracleS, MaxerS)
}
