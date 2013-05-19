package epic.framework

import breeze.linalg.DenseVector
import breeze.util.Index

/**
 * This is a standard expected counts class that most models will use...
 * Loss is the log-loss (or, whatever), and counts are for the derivative
 * @param loss
 * @param counts
 */
case class StandardExpectedCounts[F](var loss: Double,
                                  counts: DenseVector[Double],
                                  index: Index[F]) extends ExpectedCounts[StandardExpectedCounts[F]] {
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

  trait Model { this:epic.framework.Model[_] =>
    type ExpectedCounts = StandardExpectedCounts[Feature]


    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      ecounts.loss -> ecounts.counts
    }
  }

}
