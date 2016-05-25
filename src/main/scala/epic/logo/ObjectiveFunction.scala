package epic.logo

trait ObjectiveFunction[W] {

  def calculatePrimalAndDual(w : Weights[W], data : Seq[Instance[_, W]]) : (Double, Double)

}
