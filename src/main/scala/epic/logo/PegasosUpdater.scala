package epic.logo
import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef
import breeze.linalg.norm
import breeze.math.MutableInnerProductModule

class PegasosUpdater[W](C : Double)(implicit space: MutableInnerProductModule[W, Double]) extends Updater[W] {
  import space._
  def update(instance: DualVariableHolder[_, W], w : Weights[W], n : Int, iter : Int) : Boolean = {
    import instance._
    assert(constraints.length == 2)
    val (df, _) = constraints(0)
    if (norm(df) == 0.0) return false
    val t = iter + 1
    val lambda = 2.0 / C
    val eta = 1.0 / lambda / t

    w *= (1.0 - eta * lambda)
    w += df * (eta * 1.0 / n)
    val projScale = Math.min(1, 1.0 / Math.sqrt(lambda) / Math.sqrt(w.`^2`))
    w *= projScale

    return true

  }

  def currentSlack(i : DualVariableHolder[_, W], w : Weights[W]) : Double = {
    throw new UnsupportedOperationException(this.getClass().getName() + " should be only be used in online mode.")
  }

}
