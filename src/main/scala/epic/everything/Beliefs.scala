package epic.everything

import breeze.linalg._
import breeze.inference.Factor
import breeze.util.Index

/**
 * 
 * @author dlwh
 */
final case class Beliefs[T](property: Property[T], beliefs: DenseVector[Double]) extends Factor[Beliefs[T]] {

  def apply(i: Int) = beliefs(i)

  def beliefFor(i: T) = apply(property.choices(i))

  def *(f: Beliefs[T]): Beliefs[T] = if (beliefs eq null) f else if (f.beliefs eq null) this else copy(beliefs = Beliefs.stripNaNs(beliefs :* f.beliefs, beliefs, f.beliefs))
  def /(f: Beliefs[T]): Beliefs[T] = if (f.beliefs eq null) this else if (beliefs eq f.beliefs) Beliefs(property, null) else copy(beliefs = Beliefs.safeDiv(property.index, beliefs, f.beliefs))

  def logPartition: Double = {
    val sum = breeze.linalg.sum(beliefs)
    if (sum == beliefs.length && norm(beliefs-1.0) == 0.0) 0.0
    else math.log(sum)
  }

  def isConvergedTo(f: Beliefs[T], diff: Double): Boolean = {
    var i = 0
    while(i < beliefs.size) {
      if(math.abs(f.beliefs(i) - beliefs(i)) > diff) return false
      i += 1
    }
    true
  }

  def maxChange(f: Beliefs[T]): Double = {
    var i = 0
    var maxChange = 0.0
    while(i < beliefs.size) {
      maxChange = math.max(math.abs(f.beliefs(i) - beliefs(i)), maxChange)
      i += 1
    }
    maxChange
  }

  def updated(newBeliefs: DenseVector[Double]) = Beliefs(property, newBeliefs)

  def size = property.size
}

object Beliefs {
  def improperUninformed[T](name: String, index: Index[T]): Beliefs[T] = improperUninformed(Property(name, index))
  def improperUninformed[T](prop: Property[T]): Beliefs[T] = Beliefs(prop, DenseVector.ones(prop.index.size))

  private def stripNaNs(beliefs: DenseVector[Double], a: DenseVector[Double], b: DenseVector[Double]) = {
    var i = 0
    var p = beliefs.offset
    while(i < beliefs.length) {
      if (java.lang.Double.isNaN(beliefs(p)) || java.lang.Double.isInfinite(beliefs(p))) {
        if (java.lang.Double.isNaN(beliefs(p))) if(a(p) > 1E-5 || b(p) > 1E-5) throw new RuntimeException(s"Something is wrong with this division! $a $b")
        beliefs(p) = 0.0
      }
      p += beliefs.stride
      i += 1
    }

    beliefs
  }

  private def safeDiv[T](index: Index[T], a: DenseVector[Double], b: DenseVector[Double]) = {
    var i = 0
    val beliefs = DenseVector.zeros[Double](a.length)
    while(i < beliefs.length) {
      val result =  if (a(i) == 0.0 && b(i) != 0.0) {
        0.0
      } else if (a(i) != 0.0 && b(i) == 0.0) {
//        println("WTF?" + " " + a(i) + " " + b(i))
        a(i)
      } else if (a(i) == 0.0 && b(i) == 0.0) {
        0.0
      } else {
        a(i) / b(i)
      }
      beliefs(i) = result
      i += 1
    }

    beliefs
  }
}
