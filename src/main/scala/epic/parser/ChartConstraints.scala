package epic.parser

import epic.pruning.SpanConstraints
import scala.collection.BitSet
import breeze.collection.mutable.TriangularArray

@SerialVersionUID(1L)
case class ChartConstraints[-L](top: SpanConstraints[L],
                                bot: SpanConstraints[L]) extends Serializable {
  def isActiveSpan(begin: Int, end: Int):Boolean = top.isAllowedSpan(begin, end) || bot.isAllowedSpan(begin, end)
  def hasMaximalLabel(begin: Int, end: Int):Boolean = ???
}

object ChartConstraints {
  def noSparsity[L] = ChartConstraints(SpanConstraints.noConstraints[L], SpanConstraints.noConstraints[L])

  def apply[L](top: TriangularArray[_ <: BitSet], bot: TriangularArray[_ <: BitSet]): ChartConstraints[L] = ChartConstraints(SpanConstraints(top), SpanConstraints(bot))
}
