package epic.logo

import scala.collection.Seq

class FixedIterationConvergenceChecker[W](val maxNumIters : Int) extends ConvergenceChecker[W] {

  def converged(weights : Weights[W], data : Seq[DualVariableHolder[_, W]], iter : Int, numNewConstraints : Int) : Boolean = {
    iter >= maxNumIters
  }

}
