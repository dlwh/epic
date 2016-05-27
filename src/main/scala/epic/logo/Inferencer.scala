package epic.logo

trait Inferencer[S] {
  def initialState: S
  def reduceStates(state1: S, state2: S): S

}
