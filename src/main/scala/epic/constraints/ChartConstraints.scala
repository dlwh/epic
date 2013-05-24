package epic.constraints

import scala.collection.BitSet
import breeze.collection.mutable.TriangularArray
import epic.trees.{UnaryTree, BinarizedTree}
import breeze.util.Index

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

  def fromTree[L](labelIndex: Index[L], tree: BinarizedTree[L]): ChartConstraints[L] = {
    val top = TriangularArray.fill(tree.end+1){ null: BitSet }
    val bot = TriangularArray.fill(tree.end+1){ null: BitSet }
    for(t <- tree.allChildren) t match {
      case UnaryTree(p,_,_,span) =>
        top(span.begin,span.end) = BitSet(labelIndex(p))
      case _ =>
        bot(t.begin,t.end) = BitSet(labelIndex(t.label))
    }

    ChartConstraints(LabeledSpanConstraints(top), LabeledSpanConstraints(bot))
  }
}
