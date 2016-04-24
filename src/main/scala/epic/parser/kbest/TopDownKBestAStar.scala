package epic.parser
package kbest

import epic.trees._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import Trees.Zipper._
import epic.trees.Trees.Zipper

object TopDownKBestAStar {
  private implicit def ordTKAItem[L]: Ordering[TKAItem[L]] = Ordering[Double].on((_:TKAItem[L]).weight)

  def apply[L, W](chart: RefinedChartMarginal[L, W], k: Int):IndexedSeq[(BinarizedTree[L], Double)] = {
    import chart._
    val root = chart.topology.rootIndex
    val kbestList = new ArrayBuffer[(BinarizedTree[L], Double)]()
    val queue = new mutable.PriorityQueue[TKAItem[(Int, Int)]]
    queue.enqueue(StartItem)
    while (queue.nonEmpty && kbestList.size < k) {
      queue.dequeue() match {
        case StartItem =>
          val begin = 0
          val end = length
          val span = Span(begin, end)
          val l = root
          for(lref <- inside.top.enteredLabelRefinements(begin, end, l)) {
            val insideScore = inside.top.labelScore(begin, end, l, lref)
            queue += TopItem(Zipper(NullaryTree(l -> lref, span)), insideScore)
          }
        case CompleteTreeItem(tree, weight) =>
          kbestList += (tree.map(l => chart.topology.labelIndex.get(l._1)) -> weight)
        case TopItem(zipper,  weight) =>
          val (a, refA) = zipper.tree.label
          val begin = zipper.tree.begin
          val end = zipper.tree.end
          val aScore = inside.top.labelScore(begin, end, a, refA)
          for (r <- topology.indexedUnaryRulesWithParent(a); refR <- anchoring.validRuleRefinementsGivenParent(begin, end, r, refA)) {
            val b = topology.child(r)
            val chain = topology.chain(r)
            val refB = anchoring.childRefinement(r, refR)
            val bScore = inside.bot.labelScore(begin, end, b, refB)
            if (!bScore.isInfinite) {
              val rScore = anchoring.scoreUnaryRule(begin, end, r, refR)
              val newWeight = weight - aScore + bScore + rScore
              val newParentLabel = (b,refB)
              val newZipper = zipper.copy(UnaryTree(zipper.tree.label, NullaryTree(newParentLabel, zipper.tree.span), chain, zipper.tree.span)).down.get
              assert(newZipper.label == newZipper.label)
              queue += BotItem(newZipper, newWeight)
            }
          }
          // completed the whole sentence
        case BotItem(zipper, weight) if zipper.begin == zipper.end - 1 =>
          zipper.next match {
            case None =>
              queue += CompleteTreeItem(zipper.upToRoot.tree, weight)
            case Some(zip) =>
              queue += TopItem(zip, weight)
          }
        case BotItem(zipper, weight) =>
          val (root, rootRef) = zipper.label
          val begin = zipper.begin
          val end = zipper.end
          val aScore = inside.bot.labelScore(begin, end, root, rootRef)

        val traceOn = begin == 0 && end == 4
          val spanScore = anchoring.scoreSpan(begin, end, root, rootRef)
          for {
            r <- topology.indexedBinaryRulesWithParent(root)
            b = topology.leftChild(r)
            c = topology.rightChild(r)
            refR <- anchoring.validRuleRefinementsGivenParent(begin, end, r, rootRef)
            refB = anchoring.leftChildRefinement(r, refR)
            refC = anchoring.rightChildRefinement(r, refR)
            split <- inside.top.feasibleSplitPoints(begin, end, b, refB, c, refC)
          } {
            val ruleScore = anchoring.scoreBinaryRule(begin, split, end, r, refR)
            val score = (
              ruleScore
                + inside.top.labelScore(begin, split, b, refB)
                + inside.top.labelScore(split, end, c, refC)
                + spanScore
              )
            assert(score <= aScore + 1E-4, score -> aScore)
            val newWeight = weight - aScore + score
            if (!newWeight.isInfinite) {
              val newZipper = zipper.copy(BinaryTree(zipper.tree.label,
                NullaryTree(b -> refB, Span(begin,split)),
                NullaryTree(c -> refC, Span(split, end)), zipper.tree.span)).down.get
              assert(newZipper.next.get.begin == newZipper.end, newZipper)
              queue += TopItem(newZipper, newWeight)
            }
          }
      }
    }
    kbestList
  }

  /**
   *
   * @author dlwh
   */
  private sealed trait TKAItem[+L] { def weight: Double }
  private case object StartItem extends TKAItem[Nothing] { def weight = 0.0 }
  private case class TopItem[L](zipper: Zipper[L], weight: Double) extends TKAItem[L]
  private case class BotItem[L](zipper: Zipper[L], weight: Double) extends TKAItem[L]
  private case class CompleteTreeItem[L](tree: BinarizedTree[L], weight: Double) extends TKAItem[L]

}



