package epic.parser

import breeze.util.Index
import breeze.linalg.{softmax, max, DenseMatrix}
import breeze.collection.mutable.TriangularArray

/**
 * TODO
 *
 * @author dlwh
 **/
final case class SimpleChartMarginal[L, L2, W](anch: SimpleRefinedGrammar.Anchoring[L, L2, W],
                                               inside: SimpleParseChart[L2], outside: SimpleParseChart[L2],
                                               isMaxMarginal: Boolean = true) extends ParseMarginal[L, W] {
  val anchoring = AugmentedAnchoring.fromRefined(anch)

  override val logPartition: Double = {
    val scores = anch.refinements.labels.refinementsOf(anchoring.topology.rootIndex).map(inside.top.labelScore(0, length, _))
    if(isMaxMarginal) max(scores)
    else softmax(scores)
  }

  override def insideTopScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    inside.top.labelScore(begin, end, anch.refinements.labels.globalize(sym, ref))
  }

  override def insideBotScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    inside.top.labelScore(begin, end, anch.refinements.labels.globalize(sym, ref))
  }

  override def feasibleSplitPoints(begin: Int, end: Int, leftChild: Int, leftChildRef: Int, rightChild: Int, rightChildRef: Int): IndexedSeq[Int] = {
    (begin + 1) until (end)
  }

  override def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double): Unit = {
    if(logPartition.isInfinite) throw new RuntimeException("No parse for " + words)
    if(logPartition.isNaN) throw new RuntimeException("NaN prob!")

    val lexLoc = anchoring.core.tagConstraints
    // handle lexical
    for (i <- 0 until words.length) {
      var visitedSomething  = false
      for {
        a <- lexLoc.allowedTags(i)
        ref <- anchoring.refined.validLabelRefinements(i, i+ 1, a)
      } {
        val aa = anch.refinements.labels.globalize(a, ref)
        val score:Double = anchoring.scoreSpan(i, i+1, a, ref) + outside.bot(i, i+1, aa) - logPartition
        assert(!score.isNaN, s"${anchoring.scoreSpan(i, i + 1, a, ref)} ${outside.bot(i, i + 1, aa)} $logPartition")
        if (score != Double.NegativeInfinity) {
          spanVisitor.visitSpan(i, i+1, a, ref, math.exp(score))
          visitedSomething = true
        }
      }


    }

    // handle binaries
    for {
      span <- 2 to length
      begin <- 0 to (length - span)
      parent <- 0 until anch.topology.labelIndex.size
    } {
      val end = begin + span
      val aOutside = outside.bot(begin, end, parent)
      val labelMarginal = inside.bot(begin, end, parent) + aOutside - logPartition
      if(labelMarginal > spanThreshold) {
        val aCoarse = anch.refinements.labels.project(parent)
        val aRef = anch.refinements.labels.localize(parent)
        spanVisitor.visitSpan(begin, end, aCoarse, aRef, math.exp(labelMarginal))
        if(!spanVisitor.skipBinaryRules) {
          val rules = anch.refinedTopology.indexedBinaryRulesWithParent(parent)
          var i = 0
          while(i < rules.length) {
            val r = rules(i)
            val b = topology.leftChild(r)
            val c = topology.rightChild(r)

            var split = begin + 1
            while(split < end) {
              val bInside = inside.top.labelScore(begin, split, b)
              val cInside = inside.top.labelScore(split, end, c)
              val ruleScore = anch.grammar.ruleScore(r)

              val coarseR = anch.refinements.rules.project(r)
              val refR = anch.refinements.rules.localize(r)

              val margScore = bInside + cInside + ruleScore + aOutside - logPartition

              if(margScore != Double.NegativeInfinity) {
                spanVisitor.visitBinaryRule(begin, split, end, coarseR, refR, math.exp(margScore))
              }

              split += 1
            }

            i += 1
          }

        }

      }
    }

    if(!spanVisitor.skipUnaryRules)
      for {
        span <- 1 to words.length
        begin <- 0 to (words.length - span)
        end = begin + span
        parent <- 0 until anch.topology.labelIndex.size
      } {
        val end = begin + span
        val aOutside = outside.top(begin, end, parent)
        val labelMarginal = inside.top(begin, end, parent) + aOutside - logPartition
        if (labelMarginal > spanThreshold) {

          for (r <- anch.refinedTopology.indexedUnaryRulesWithParent(parent)) {
            val b = topology.child(r)
            val bScore = inside.bot.labelScore(begin, end, b)
            val rScore = anch.grammar.ruleScore(r)
            val prob = math.exp(bScore + aOutside + rScore - logPartition)
            val refR = anch.refinements.rules.localize(r)
            val projR = anch.refinements.rules.project(r)
            if (prob > 0)
              spanVisitor.visitUnaryRule(begin, end, projR, refR, prob)
          }
        }
      }

  }
}


@SerialVersionUID(1)
final class SimpleParseChart[L](val index: Index[L], val length: Int) extends Serializable {

  val top, bot = new ChartScores()

  final class ChartScores private[SimpleParseChart]() {
    val scores = DenseMatrix.zeros[Double](index.size, TriangularArray.arraySize(length + 1))
    scores := Double.NegativeInfinity
    @inline def labelScore(begin: Int, end: Int, label: Int) = scores(label, TriangularArray.index(begin, end))
    @inline def apply(begin: Int, end: Int, label: Int) = scores(label, TriangularArray.index(begin, end))

    def enter(begin: Int, end: Int, parent: Int, w: Double): Unit = {
      scores(parent, TriangularArray.index(begin, end)) = w
    }

  }

}