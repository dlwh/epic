package epic.logo

trait ConvergenceChecker[W] {

  def converged(weights : Weights[W], data : Seq[Instance[_, W]], iter : Int, numNewConstraints : Int) : Boolean

}
