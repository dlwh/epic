package epic.logo

trait Inferencer[S] {
  /** Inferencers may wish to carry state between calls. Rather than maintaining it internally
   *  and breaking referentially transparency, the may return states after each call that
   *  are aggregated together using [[reduceStates]].
   */
  def initialState: S
  def reduceStates(state1: S, state2: S): S

}
