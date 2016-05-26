package epic.logo

import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef

trait Updater[W] {

  def update(constraints: IndexedSeq[(W, Double)], alphas: Buffer[Double], slack: DoubleRef,
             weights: Weights[W], n: Int, iter: Int): Boolean

  def currentSlack(i : Instance[_, W], w : Weights[W]) : Double

}
