package epic.constraints

import scala.collection.BitSet
import breeze.collection.mutable.TriangularArray

/**
 * Has constraints relevant to building an [[epic.parser.ParseChart]],
 * which is to say [[epic.constraints.LabeledSpanConstraints]] for
 * top and bot cells.
 * @param top constraints for the top symbols of the parsechart
 * @param bot constraints for the bottom symbols of the parsechart
 * @tparam L
 */
@SerialVersionUID(1L)
case class ChartConstraints[-L](top: LabeledSpanConstraints[L],
                                bot: LabeledSpanConstraints[L]) extends SpanConstraints with Serializable {

  def isAllowedSpan(begin: Int, end: Int):Boolean = top.isAllowedSpan(begin, end) || bot.isAllowedSpan(begin, end)
  /** TODO */ // TODO
  def hasMaximalLabel(begin: Int, end: Int):Boolean = ???


  def maxSpanLengthStartingAt(begin: Int): Int = top.maxSpanLengthStartingAt(begin) max bot.maxSpanLengthStartingAt(begin)

  def flatten = top | bot


}

object ChartConstraints {
  def noSparsity[L] = ChartConstraints(LabeledSpanConstraints.noConstraints[L], LabeledSpanConstraints.noConstraints[L])

  def apply[L](top: TriangularArray[_ <: BitSet], bot: TriangularArray[_ <: BitSet]): ChartConstraints[L] = ChartConstraints(LabeledSpanConstraints(top), LabeledSpanConstraints(bot))

  trait Factory[L, W] extends SpanConstraints.Factory[W] {
    def constraints(w: IndexedSeq[W]): ChartConstraints[L]
  }
}
