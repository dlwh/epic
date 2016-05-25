package epic.logo

import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef
import scala.util.Random

class L1Updater[W](C : Double) extends Updater[W] {
   val shuffleRand = new Random()

  def update(constraints : IndexedSeq[(FeatureVector[W], Double)], alphas : Buffer[Double], slack : DoubleRef, w : Weights[W], n : Int, iter : Int) : Boolean = {
    if (constraints.length == 1) {
      if (alphas(0) != C) {
        val eta = C - alphas(0)
        val (df, l) = constraints(0)
        if (eta != 0.0) {
          alphas(0) += eta
          w += df * eta
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
          val denom = diff ^ 2
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

  def currentSlack(i : Instance[_, W], w : Weights[W]) = {
    L1Objective.slack(i, w)
  }
}
