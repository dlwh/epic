package epic.framework

import breeze.linalg.DenseVector
import breeze.util.Index

/**
 * This is a standard expected counts class that most models will use...
 * Loss is the log-loss (or, whatever), and
 * @param loss
 * @param counts
 */
case class StandardExpectedCounts(var loss: Double,
                                  counts: DenseVector[Double]) extends ExpectedCounts[StandardExpectedCounts] {
  def +=(that: StandardExpectedCounts): StandardExpectedCounts = {
    this.loss += that.loss; this.counts += that.counts; this
  }

  def -=(that: StandardExpectedCounts): StandardExpectedCounts = {
    this.loss -= that.loss; this.counts -= that.counts; this
  }
}

object StandardExpectedCounts {
  def zero(index: Index[_]) = StandardExpectedCounts(0.0, DenseVector.zeros(index.size))

  trait Model { this:epic.framework.Model[_] =>
    type ExpectedCounts = StandardExpectedCounts

    def emptyCounts = zero(featureIndex)

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      ecounts.loss -> ecounts.counts
    }
  }
}
