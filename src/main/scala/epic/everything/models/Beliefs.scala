package epic.everything.models

import breeze.linalg._
import breeze.inference.Factor

/**
 * 
 * @author dlwh
 */
final case class Beliefs[T](property: Property[T], beliefs: DenseVector[Double]) extends Factor[Beliefs[T]] {
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

  def newBuilder = Beliefs.Builder(property)
}

object Beliefs {
  case class Builder[T](property: Property[T]) {
    val counts = property.mkDenseVector()
    def tallyCount(assignment: Int, count: Double) {
      counts(assignment) += count
    }
    def result() = Beliefs(property, normalize(counts, 1.0))
  }
}
