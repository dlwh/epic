package epic.everything.models

import breeze.linalg.DenseVector
import breeze.inference.Factor

/**
 * 
 * @author dlwh
 */
case class Beliefs[T](property: Property[T], beliefs: DenseVector[Double]) extends Factor[Beliefs[T]] {
  def apply(i: Int) = beliefs(i)

  def beliefFor(i: T) = apply(property.choices(i))

  def *(f: Beliefs[T]): Beliefs[T] = copy(beliefs=beliefs :* f.beliefs)
  def /(f: Beliefs[T]): Beliefs[T] = copy(beliefs=beliefs :/ f.beliefs)

  def logPartition: Double = beliefs.sum

  def isConvergedTo(f: Beliefs[T], diff: Double): Boolean = {
    var i = 0
    while(i < beliefs.size) {
      if(math.abs(f.beliefs(i) - beliefs(i)) > diff) return false
      i += 1
    }
    true
  }
}
