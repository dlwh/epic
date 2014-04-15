package epic.framework

import breeze.linalg.{scaleAdd, DenseVector, axpy}
import breeze.util.Index
import breeze.features.FeatureVector
import breeze.generic.UFunc.InPlaceImpl3

/**
 * This is a standard expected counts class that most models will use...
 * Loss is the log-loss (or, whatever), and counts are for the derivative
 * @param loss
 * @param counts
 */
case class StandardExpectedCounts[F](var loss: Double,
                                  counts: DenseVector[Double],
                                  index: Index[F]) extends ExpectedCounts[StandardExpectedCounts[F]] {

  def length = index.size

  def toObjective: (Double, DenseVector[Double]) = loss -> counts

  def +=(that: StandardExpectedCounts[F]): StandardExpectedCounts[F] = {
    this.loss += that.loss; this.counts += that.counts; this
  }

  def -=(that: StandardExpectedCounts[F]): StandardExpectedCounts[F] = {
    this.loss -= that.loss; this.counts -= that.counts; this
  }

  def *=(scale: Double) = {
    loss *= scale
    counts *= scale
    this
  }

  def apply(f: F) = counts(index(f))
}

object StandardExpectedCounts {
  def zero[F](index: Index[F]) = StandardExpectedCounts(0.0, DenseVector.zeros(index.size), index)

  trait Model[D] extends epic.framework.Model[D] {
    type ExpectedCounts = StandardExpectedCounts[Feature]

    def emptyCounts = zero(this.featureIndex)

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      ecounts.loss -> ecounts.counts
    }
  }

  implicit def scaleAddCounts[T]: InPlaceImpl3[scaleAdd.type, StandardExpectedCounts[T], Double, FeatureVector]  = new scaleAdd.InPlaceImpl3[StandardExpectedCounts[T], Double, FeatureVector] {
    override def apply(v: StandardExpectedCounts[T], v2: Double, v3: FeatureVector): Unit = {
      axpy(v2, v3, v.counts)
    }
  }

}
