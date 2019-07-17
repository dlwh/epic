package epic.constraints

import epic.lexicon.Lexicon
import epic.util.Has2
import epic.trees.{AnnotatedLabel, BinarizedTree}
import breeze.util.Index
import breeze.collection.mutable.TriangularArray
import epic.constraints.LabeledSpanConstraints.PromotedSpanConstraints

/**
 *
 * @author dlwh
 */
trait SpanConstraints { outer =>
  def apply(begin: Int, end: Int): Boolean = isAllowedSpan(begin, end)

  def isAllowedSpan(begin: Int, end: Int): Boolean
  def maxSpanLengthStartingAt(begin: Int): Int

  def |(other: SpanConstraints):SpanConstraints = new SpanConstraints {
    def isAllowedSpan(begin: Int, end: Int): Boolean = outer.isAllowedSpan(begin, end) || other.isAllowedSpan(begin, end)
    def maxSpanLengthStartingAt(begin: Int): Int = outer.maxSpanLengthStartingAt(begin) max other.maxSpanLengthStartingAt(begin)
  }

  def &(other: SpanConstraints):SpanConstraints = new SpanConstraints {
    def isAllowedSpan(begin: Int, end: Int): Boolean = outer.isAllowedSpan(begin, end) && other.isAllowedSpan(begin, end)
    def maxSpanLengthStartingAt(begin: Int): Int = outer.maxSpanLengthStartingAt(begin) min other.maxSpanLengthStartingAt(begin)
  }
}

object SpanConstraints {

  def fromTree[L](tree: BinarizedTree[L]): SpanConstraints = {
    val allowedSpans = new java.util.BitSet()
    for(t <- tree.allChildren) {
        allowedSpans.set(TriangularArray.index(t.begin, t.end))
    }

    val constraints = new SpanConstraints {
      def maxSpanLengthStartingAt(begin: Int): Int =  tree.end-tree.begin

      def isAllowedSpan(begin: Int, end: Int): Boolean = allowedSpans.get(TriangularArray.index(begin, end))
    }

    constraints
  }

  trait Factory[W] extends Has2[IndexedSeq[W], SpanConstraints] {
    def constraints(w: IndexedSeq[W]):SpanConstraints
    def get(h: IndexedSeq[W]): SpanConstraints = constraints(h)

    def |(other: SpanConstraints.Factory[W]) = new UnionFactory(this, other)
  }

  @SerialVersionUID(1L)
  class UnionFactory[W](factories: Factory[W]*) extends Factory[W] with Serializable {
    def constraints(w: IndexedSeq[W]): SpanConstraints = factories.map(_.constraints(w)).reduce(_ | _)
  }

  object Factory {
    def noConstraints[W]:Factory[W] = new Factory[W] {
      def constraints(w: IndexedSeq[W]): SpanConstraints = LabeledSpanConstraints.noConstraints
    }
  }

}
