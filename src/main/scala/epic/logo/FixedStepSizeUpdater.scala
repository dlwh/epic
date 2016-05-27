package epic.logo
import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef
import breeze.linalg.norm
import breeze.math.MutableInnerProductModule

class FixedStepSizeUpdater[W](stepSize : Int => Double, C : Double)(implicit space: MutableInnerProductModule[W, Double]) extends Updater[W] {
  import space._

  def update(instance: DualVariableHolder[_, W],
             w: Weights[W], n: Int, iter: Int): Boolean = {
    import instance._
    assert(constraints.length == 2)
    val (df, _) = constraints(0)
    if (norm(df) == 0.0) return false

    val eta = stepSize(iter)
    if (C == Double.PositiveInfinity) {
      w += df * eta
    } else {
      w *= (1.0 - eta)
      w += df * (eta * C / n)
    }
    return true
  }

  def currentSlack(i : DualVariableHolder[_, W], w : Weights[W]) : Double = {
    throw new UnsupportedOperationException(this.getClass().getName() + " should be only be used in online mode.")
  }

}
