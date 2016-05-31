package epic.logo

import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef
import breeze.math.MutableInnerProductModule

class L2Updater[W](C : Double)(implicit space: MutableInnerProductModule[W, Double]) extends Updater[W] {
  import space._

  def update(instance: DualVariableHolder[_, W],
             w: Weights[W], n: Int, iter: Int): Boolean = {
    import instance._
    for (((df, l), s) <- constraints.zipWithIndex) {
      val wTdf = w * df
      val num = l - wTdf - slack
      if (num != 0.0) {
        val denom = (1.0 / C) + (df dot df)
        if (denom != 0.0) {
          val eta = clip(num / denom, -alphas(s), Double.PositiveInfinity)
          if (eta != 0.0) {
            alphas(s) += eta
            slack += eta / C
            w += df * eta
            return true

          }
        }
      }
    }
    return false

  }

  def currentSlack(i : DualVariableHolder[_, W], w : Weights[W]) = {
    i.slack
  }

}
