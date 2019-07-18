package epic.logo

class CompoundIterationCallback[T, W, S1, S2](val callbacks: Iterable[IterationCallback[T, W, S1, S2]])
    extends IterationCallback[T, W, S1, S2] {

  override def startIteration(iter: Int, weights: Weights[W]): Unit = {
    callbacks.foreach(_.startIteration(iter, weights))
  }

  override def startMinibatch(iter: Int, weights: Weights[W],
                              miniBatch: Array[MinibatchInput[T, W]]): Unit = {
    callbacks.foreach(_.startMinibatch(iter, weights, miniBatch))
  }
  override def endMinibatch(iter: Int, weights: Weights[W],
                            miniBatch: Array[MinibatchOutput[T, W, S1, S2]]): Unit = {
    callbacks.foreach(_.endMinibatch(iter, weights, miniBatch))
  }

  override def endIteration(iter: Int, weights: Weights[W], state1: S1, state2: S2): Unit = {
    callbacks.foreach(_.endIteration(iter, weights, state1, state2))
  }

  override def objectiveValCheck(primal: Double, dual: Double, iter: Int, w: Weights[W]): Unit = {
    callbacks.foreach(_.objectiveValCheck(primal, dual, iter, w))
  }

  override def converged(weights: Weights[W], data: Seq[DualVariableHolder[T, W]],
                         iter: Int, numNewConstraints: Int): Boolean = {
    callbacks.forall(_.converged(weights, data, iter, numNewConstraints))
  }

}
