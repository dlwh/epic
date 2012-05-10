package scalanlp.parser

import scalala.tensor.dense.DenseVectorCol
import scalanlp.util.{Encoder, Index}

final case class ExpectedCounts[Feat](index: Index[Feat],
                                      counts: DenseVectorCol[Double],
                                      var loss: Double) extends epic.ExpectedCounts[ExpectedCounts[Feat]] {

  def this(index: Index[Feat]) = this(index, DenseVectorCol.zeros(index.size), 0.0)

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
