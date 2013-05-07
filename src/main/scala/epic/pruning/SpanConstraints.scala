package epic.pruning

import scala.collection.BitSet
import breeze.collection.mutable.TriangularArray
import epic.pruning.SpanConstraints.{SimpleConstraints, NoConstraints}

/**
 * Represents
 *
 * @author dlwh
 */
sealed trait SpanConstraints[-L] {
  def isAllowedSpan(begin: Int, end: Int): Boolean
  def isAllowedLabeledSpan(begin: Int, end: Int, label: Int): Boolean

  def &(other: SpanConstraints[L]): SpanConstraints[L] = this match {
    case NoConstraints => this
    case SimpleConstraints(x) => other match {
      case NoConstraints => other
      case SimpleConstraints(y) =>
        require(x.dimension == y.dimension, "Dimensions of constrained spans must match!")
        SimpleConstraints(TriangularArray.tabulate(x.dimension) { (b,e) =>
          x(b,e) & y(b,e)
        })
    }
  }

  def |(other: SpanConstraints[L]): SpanConstraints[L] = this match {
    case NoConstraints => other
    case SimpleConstraints(x) => other match {
      case NoConstraints => this
      case SimpleConstraints(y) =>
        require(x.dimension == y.dimension, "Dimensions of constrained spans must match!")
        SimpleConstraints(TriangularArray.tabulate(x.dimension) { (b,e) =>
          x(b,e) | y(b,e)
        })
    }
  }
}

object SpanConstraints {
  def noConstraints[L]:SpanConstraints[L] = NoConstraints
  def apply[L](spans: TriangularArray[_ <: BitSet]):SpanConstraints[L] = SimpleConstraints(spans)

  @SerialVersionUID(1L)
  object NoConstraints extends SpanConstraints[Any] with Serializable {
    def isAllowedSpan(begin: Int, end: Int): Boolean = true
    def isAllowedLabeledSpan(begin: Int, end: Int, label: Int): Boolean = true
  }

  case class SimpleConstraints[L](spans: TriangularArray[_ <:BitSet]) extends SpanConstraints[L] {
    def isAllowedSpan(begin: Int, end: Int): Boolean = (spans(begin,end) ne null) && spans(begin,end).nonEmpty

    def isAllowedLabeledSpan(begin: Int, end: Int, label: Int): Boolean = (spans(begin,end) ne null) && spans(begin, end).contains(label)
  }


}
