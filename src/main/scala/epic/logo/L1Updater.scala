package epic.logo

import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef
import scala.util.Random
import breeze.math.MutableInnerProductModule

class L1Updater[W](C : Double)(implicit space: MutableInnerProductModule[W, Double]) extends Updater[W] {
  val shuffleRand = new Random(1)
  import space._

  def update(instance: DualVariableHolder[_, W], w : Weights[W], n : Int, iter : Int) : Boolean = {
    import instance._
    if (constraints.length == 1) {
      val alpha0 = alphas(0)
      if (alpha0 != C) {
        val eta = C - alpha0
        val (df, l) = constraints(0)
        if (eta != 0.0) {
          w += df * eta
          alphas(0) += eta
          return true
        }
      }
      return false
    } else {
      var anyChange = false
      val indexed = constraints.zipWithIndex
      val ((df1, l1), s1) = indexed.maxBy { case ((df, l), s) => l - w * df }

      for (((df2, l2), s2) <- shuffleRand.shuffle(indexed) if s2 != s1 && alphas(s2) > 0.0) {
        val diff = df2 - df1
        val num = (l2 - l1) - w * diff
        if (num != 0.0) {
          val denom = diff dot diff
          if (denom != 0.0) {
            val eta = clip(num / denom, -alphas(s2), +alphas(s1))
            if (eta != 0.0) {
              alphas(s2) += eta
              alphas(s1) -= eta
              w += diff * eta
              anyChange = true
            }
          }
        }

      }
      return anyChange
    }
  }

  def currentSlack(i : DualVariableHolder[_, W], w : Weights[W]) = {
    L1Objective.slack(i, w)
  }
}
