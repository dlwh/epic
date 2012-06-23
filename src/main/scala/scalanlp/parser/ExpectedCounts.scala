package scalanlp.parser

import breeze.linalg._
import breeze.util.{Encoder, Index}

/**
 * Used to count the occurences of features in a set
 * of marginals. Works closely with a [[scalanlp.parser.RefinedFeaturizer]]
 * to actually compute expected counts. This just tallies them.
 * @param index index over features corresponding to counts' size
 * @param counts feature counts, encoded to a vector using index
 * @param loss usually log-loss, which is basically negative log likelihood
 * @tparam Feat
 */
final case class ExpectedCounts[Feat](index: Index[Feat],
                                      counts: DenseVector[Double],
                                      var loss: Double) extends scalanlp.framework.ExpectedCounts[ExpectedCounts[Feat]] {

  def this(index: Index[Feat]) = this(index, DenseVector.zeros(index.size), 0.0)

  def decode = Encoder.fromIndex(index).decode(counts)

  def +=(c: ExpectedCounts[Feat]) = {
    val ExpectedCounts(_, wCounts, tProb) = c

    this.counts += wCounts

    loss += tProb
    this
  }

  def -=(c: ExpectedCounts[Feat]) = {
    val ExpectedCounts(_, cc, tProb) = c

    this.counts -= cc

    loss -= tProb
    this
  }

  /**
   * Gets the count of the feature.
   */
  def apply(f: Feat) = {
    counts(index(f))
  }
}
