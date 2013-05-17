package epic.constraints

import epic.lexicon.Lexicon
import epic.util.Has2

/**
 *
 * @author dlwh
 */
trait SpanConstraints { outer =>
  def apply(begin: Int, end: Int):Boolean = isAllowedSpan(begin, end)

  def isAllowedSpan(begin: Int, end: Int): Boolean
  def maxSpanLengthStartingAt(begin: Int):Int

  def |(other: SpanConstraints):SpanConstraints = new SpanConstraints {
    def isAllowedSpan(begin: Int, end: Int): Boolean = outer.isAllowedSpan(begin, end) || other.isAllowedSpan(begin, end)
    def maxSpanLengthStartingAt(begin: Int):Int = outer.maxSpanLengthStartingAt(begin) max other.maxSpanLengthStartingAt(begin)
  }
}

object SpanConstraints {
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
    def fromLexicon[L,W](lexicon: Lexicon[L, W]):Factory[W] = new Factory[W] {
      def constraints(w: IndexedSeq[W]): SpanConstraints = LabeledSpanConstraints.fromTagConstraints(lexicon.anchor(w))
    }

    def noConstraints[W]:Factory[W] = new Factory[W] {
      def constraints(w: IndexedSeq[W]): SpanConstraints = LabeledSpanConstraints.noConstraints
    }
  }

}
