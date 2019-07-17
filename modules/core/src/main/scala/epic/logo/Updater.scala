package epic.logo

import scala.collection.mutable.Buffer
import scala.runtime.DoubleRef

trait Updater[W] {

  /**
   * Mutates `instance` and `weights`.
   */
  def update(instance: DualVariableHolder[_, W],
             weights: Weights[W], n: Int, iter: Int): Boolean

  def currentSlack(i: DualVariableHolder[_, W], w: Weights[W]): Double

}
