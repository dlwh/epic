package epic.logo
import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef

class FixedStepSizeUpdater[W](stepSize : Int => Double, C : Double) extends Updater[W] {

  def update(constraints: IndexedSeq[(FeatureVector[W], Double)], alphas: Buffer[Double], slack: DoubleRef,
             w: Weights[W], n: Int, iter: Int): Boolean = {
    assert(constraints.length == 2)
    val (df, _) = constraints(0)
    if ((df ^ 2) == 0.0) return false

    val eta = stepSize(iter)
    if (C == Double.PositiveInfinity) {
      w += df * eta
    } else {
      w *= (1.0 - eta)
      w += df * (eta * C / n)
    }
    return true
  }

  def currentSlack(i : Instance[_, W], w : Weights[W]) : Double = {
    throw new UnsupportedOperationException(this.getClass().getName() + " should be only be used in online mode.")
  }

}
