package epic.sequences

import breeze.linalg.{SparseVector, Vector}
import breeze.collection.mutable.TriangularArray

/**
 * Created with IntelliJ IDEA.
 * User: dburkett
 * Date: 2/7/13
 * Time: 11:38 AM
 * To change this template use File | Settings | File Templates.
 */
class CompressedFeatureCache(data: Array[Array[TriangularArray[IndicatorFeatureVector]]]) {
  def apply(value: Int) = data(value)
}


class IndicatorFeatureVector(data: Array[Int]) {
  def dot(vector: Vector[Double]) = {
    var sum = 0.0
    var i = 0
    while (i < data.size) {
      sum += vector(data(i))
      i += 1
    }
    sum
  }

  def size = data.size
  def apply(v: Int) = data(v)

  override def toString = {
    "IndicatorFeatureVector:" + data.deep.mkString("(", ",", ")")
  }
}

object IndicatorFeatureVector {
  def apply(iterator: Iterator[Int]): IndicatorFeatureVector = new IndicatorFeatureVector(iterator.toArray)
  def apply(v: Seq[Int]): IndicatorFeatureVector = new IndicatorFeatureVector(v.toArray)
  def apply(v: SparseVector[Double]): IndicatorFeatureVector = IndicatorFeatureVector(for ((idx, value) <- v.activeIterator if value > 0) yield idx)
}