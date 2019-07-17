package epic.logo

trait ArgmaxInferencer[T, W, S] extends Inferencer[S] {
  type Y

  def argmax(weights : Weights[W], instance : T) : (Y, W, Double, S)
  def initialState: S
  def reduceStates(s1: S, s2: S): S
}
