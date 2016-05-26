package epic.logo

import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef
import breeze.math.MutableInnerProductModule

class L2Updater[W](C : Double)(implicit space: MutableInnerProductModule[W, Double]) extends Updater[W] {
  import space._

  def update(constraints: IndexedSeq[(W, Double)], alphas: Buffer[Double], slack: DoubleRef,
             w: Weights[W], n: Int, iter: Int): Boolean = {
    for (((df, l), s) <- constraints.zipWithIndex) {
      val wTdf = w * df
      val num = l - wTdf - slack.elem
      if (num != 0.0) {
        val denom = (1.0 / C) + (df dot df)
        if (denom != 0.0) {
          val eta = clip(num / denom, -alphas(s), Double.PositiveInfinity)
          if (eta != 0.0) {
            alphas(s) += eta
            slack.elem += eta / C
            w += df * eta
            return true

          }
        }
      }
    }
    return false

  }

  def currentSlack(i : Instance[_, W], w : Weights[W]) = {
    i.slack.elem
  }

}
