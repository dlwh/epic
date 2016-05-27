package epic.logo

case class MinibatchInput[T, W](instance : Instance[T, W], instanceNum : Int)

case class MinibatchOutput[T, W, OracleS, MaxerS](
  instance: Instance[T, W], instanceNum: Int, df: W, loss: Double,
  oracleState: OracleS, maxerState: MaxerS)
