package epic.logo

trait ObjectiveFunction[W] {

  def calculatePrimalAndDual(w : Weights[W], data : Seq[DualVariableHolder[_, W]]) : (Double, Double)

}
