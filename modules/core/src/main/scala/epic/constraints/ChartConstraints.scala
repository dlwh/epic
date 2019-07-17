package epic.constraints

import scala.collection.BitSet
import breeze.collection.mutable.TriangularArray
import epic.trees.{TreeInstance, UnaryTree, BinarizedTree}
import breeze.util.Index
import org.mapdb.Serializer
import java.io.{DataOutput, DataInput}

/**
 * Has constraints relevant to building an [[epic.parser.RefinedParseChart]],
 * which is to say [[epic.constraints.LabeledSpanConstraints]] for
 * top and bot cells.
 * @param top constraints for the top symbols of the parsechart
 * @param bot constraints for the bottom symbols of the parsechart
 * @tparam L
 */
@SerialVersionUID(1L)
case class ChartConstraints[L](top: LabeledSpanConstraints[L],
                                bot: LabeledSpanConstraints[L]) extends SpanConstraints with Serializable {

  def isAllowedSpan(begin: Int, end: Int): Boolean = top.isAllowedSpan(begin, end) || bot.isAllowedSpan(begin, end)
  /** TODO */ // TODO
  def hasMaximalLabel(begin: Int, end: Int): Boolean = ???

  def maxSpanLengthStartingAt(begin: Int): Int = top.maxSpanLengthStartingAt(begin) max bot.maxSpanLengthStartingAt(begin)

  def flatten = top | bot
  def &(other: ChartConstraints[L]) = if (this eq other) this else ChartConstraints(top & other.top, bot & other.bot)
  def |(other: ChartConstraints[L]) = ChartConstraints(top | other.top, bot | other.bot)

}

object ChartConstraints {

  def noSparsity[L]: ChartConstraints[L] = ChartConstraints[L](LabeledSpanConstraints.noConstraints[L], LabeledSpanConstraints.noConstraints[L])

  def apply[L](top: TriangularArray[_ <: BitSet], bot: TriangularArray[_ <: BitSet]): ChartConstraints[L] = ChartConstraints(LabeledSpanConstraints(top), LabeledSpanConstraints(bot))

  trait Factory[L, W] extends SpanConstraints.Factory[W] {
    def constraints(w: IndexedSeq[W]): ChartConstraints[L]
    def |(cf: Factory[L, W]) = new OrFactory(this, cf)
  }

  object Factory {
    def noSparsity[L, W]:Factory[L, W] = new NoSparsityFactory[L, W]
  }

  class NoSparsityFactory[L, W] extends Factory[L, W] with Serializable {
    override def constraints(w: IndexedSeq[W]): ChartConstraints[L] = ChartConstraints.noSparsity[L]
  }

  class GoldConstraintsFactory[L, W](labelIndex: Index[L], insts: Traversable[TreeInstance[L, W]]) extends Factory[L, W] {
    private val gold = insts.map(ti => ti.words -> fromTree(labelIndex, ti.tree)).toMap
    def constraints(w: IndexedSeq[W]): ChartConstraints[L] = {
      gold.getOrElse(w, noSparsity[L])
    }
  }

  class UnifiedFactory[L, W](f: Factory[L, W]) extends Factory[L, W] {
    def constraints(w: IndexedSeq[W]): ChartConstraints[L] = {
      val cons = f.constraints(w)
      ChartConstraints(cons.top | cons.bot, cons.top | cons.bot)
    }
  }

  class OrFactory[L, W](f: Factory[L, W], f2: Factory[L, W]) extends Factory[L, W] {
    def constraints(w: IndexedSeq[W]): ChartConstraints[L] = {
      f.constraints(w) | f2.constraints(w)
    }
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

  implicit def serializerChartConstraints[L]:Serializer[ChartConstraints[L]] = new Serializer[ChartConstraints[L]] with Serializable {
    def serialize(out: DataOutput, value: ChartConstraints[L]) {
       implicitly[Serializer[LabeledSpanConstraints[L]]].serialize(out, value.top)
       implicitly[Serializer[LabeledSpanConstraints[L]]].serialize(out, value.bot)
    }

    def deserialize(in: DataInput, available: Int): ChartConstraints[L] = {
      val top = implicitly[Serializer[LabeledSpanConstraints[L]]].deserialize(in, available)
      val bot = implicitly[Serializer[LabeledSpanConstraints[L]]].deserialize(in, available)
      ChartConstraints(top, bot)
    }
  }

}
