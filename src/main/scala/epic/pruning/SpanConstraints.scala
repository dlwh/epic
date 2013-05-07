package epic.pruning

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

}
