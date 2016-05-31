package epic.logo

trait IterationCallback[T, W, OracleS, MaxerS] {

  def startIteration(iter : Int, weights : Weights[W]) : Unit = {}

  def startMinibatch(iter : Int, weights : Weights[W], miniBatch : Array[MinibatchInput[T, W]]) : Unit = {}
  def endMinibatch(iter : Int, weights : Weights[W], miniBatch : Array[MinibatchOutput[T, W, OracleS, MaxerS]]) : Unit = {}
  def endIteration(iter : Int, weights : Weights[W], oracleState: OracleS, maxerState: MaxerS) : Unit = {}

  def objectiveValCheck(primal : Double, dual : Double, iter : Int, weights : Weights[W]) : Unit = {}

  def converged(weights : Weights[W], data : Seq[DualVariableHolder[T, W]], iter : Int, numNewConstraints : Int) : Boolean = false

}

case class NullIterationCallback[T, W, S1, S2]() extends IterationCallback[T, W, S1, S2]
