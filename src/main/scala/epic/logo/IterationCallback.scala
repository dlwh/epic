package epic.logo

trait IterationCallback[T, W] {

  def startIteration(iter : Int, weights : Weights[W]) : Unit = {}

  def startMinibatch(iter : Int, weights : Weights[W], miniBatch : Array[MinibatchInput[T, W]]) : Unit = {}
  def endMinibatch(iter : Int, weights : Weights[W], miniBatch : Array[MinibatchOutput[T, W]]) : Unit = {}
  def endIteration(iter : Int, weights : Weights[W]) : Unit = {}

  def objectiveValCheck(primal : Double, dual : Double, iter : Int, weights : Weights[W]) : Unit = {}

  def converged(weights : Weights[W], data : Seq[Instance[T, W]], iter : Int, numNewConstraints : Int) : Boolean = false

}

case class NullIterationCallback[T, W]() extends IterationCallback[T, W]
