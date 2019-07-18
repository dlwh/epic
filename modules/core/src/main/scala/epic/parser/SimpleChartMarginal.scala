package epic.parser

import breeze.util.Index
import breeze.linalg.{DenseVector, softmax, max, DenseMatrix}
import breeze.collection.mutable.TriangularArray

import spire.syntax.cfor._
import epic.constraints.ChartConstraints

/**
 * TODO
 *
 * @author dlwh
 **/
final case class SimpleChartMarginal[L, L2, W](anchoring: SimpleGrammar.Anchoring[L, L2, W],
                                               inside: SimpleParseChart[L2], outside: SimpleParseChart[L2],
                                               isMaxMarginal: Boolean = true) extends ParseMarginal[L, W] {
  override val logPartition: Double = inside.top.labelScore(0, inside.length, anchoring.refinedTopology.rootIndex)

  override def insideTopScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    inside.top.labelScore(begin, end, anchoring.refinements.labels.globalize(sym, ref))
  }

  override def insideBotScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    inside.bot.labelScore(begin, end, anchoring.refinements.labels.globalize(sym, ref))
  }

  override def feasibleSplitPoints(begin: Int, end: Int, leftChild: Int, leftChildRef: Int, rightChild: Int, rightChildRef: Int): IndexedSeq[Int] = {
    (begin + 1) until end
  }

  override def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double): Unit = {
    if (logPartition.isInfinite) throw new RuntimeException("No parse for " + words)
    if (logPartition.isNaN) throw new RuntimeException("NaN prob!")

    val refinedTopology = anchoring.refinedTopology

    val lexLoc = anchoring.lexicon.anchor(anchoring.words)
    // handle lexical
    for (i <- words.indices) {
      var visitedSomething  = false
      for {
        a <- lexLoc.allowedTags(i)
        ref <- anchoring.validLabelRefinements(i, i+ 1, a)
      } {
        val aa = anchoring.refinements.labels.globalize(a, ref)
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
      parent <- 0 until anchoring.refinedTopology.labelIndex.size
    } {
      val end = begin + span
      val aOutside = outside.bot(begin, end, parent)
      val labelMarginal = inside.bot(begin, end, parent) + aOutside - logPartition
      if (labelMarginal > spanThreshold) {
        val aCoarse = anchoring.refinements.labels.project(parent)
        val aRef = anchoring.refinements.labels.localize(parent)
        spanVisitor.visitSpan(begin, end, aCoarse, aRef, math.exp(labelMarginal))
        if (!spanVisitor.skipBinaryRules) {
          val rules = anchoring.refinedTopology.indexedBinaryRulesWithParent(parent)
          var i = 0
          while (i < rules.length) {
            val r = rules(i)
            val b = refinedTopology.leftChild(r)
            val c = refinedTopology.rightChild(r)

            var split = begin + 1
            while (split < end) {
              val bInside = inside.top.labelScore(begin, split, b)
              val cInside = inside.top.labelScore(split, end, c)
              val ruleScore = anchoring.grammar.ruleScore(r)

              val coarseR = anchoring.refinements.rules.project(r)
              val refR = anchoring.refinements.rules.localize(r)

              val margScore = bInside + cInside + ruleScore + aOutside - logPartition

              if (margScore != Double.NegativeInfinity) {
                spanVisitor.visitBinaryRule(begin, split, end, coarseR, refR, math.exp(margScore))
              }

              split += 1
            }

            i += 1
          }

        }

      }
    }

    if (!spanVisitor.skipUnaryRules)
      for {
        span <- 1 to words.length
        begin <- 0 to (words.length - span)
        end = begin + span
        parent <- 0 until anchoring.refinedTopology.labelIndex.size
      } {
        val end = begin + span
        val aOutside = outside.top(begin, end, parent)
        val labelMarginal = inside.top(begin, end, parent) + aOutside - logPartition
        if (labelMarginal > spanThreshold) {

          for (r <- anchoring.refinedTopology.indexedUnaryRulesWithParent(parent)) {
            val b = anchoring.refinedTopology.child(r)
            val bScore = inside.bot.labelScore(begin, end, b)
            val rScore = anchoring.grammar.ruleScore(r)
            val prob = math.exp(bScore + aOutside + rScore - logPartition)
            val refR = anchoring.refinements.rules.localize(r)
            val projR = anchoring.refinements.rules.project(r)
            if (prob > 0)
              spanVisitor.visitUnaryRule(begin, end, projR, refR, prob)
          }
        }
      }

  }
}

object SimpleChartMarginal  {
  import RefinedChartMarginal.{Summer, MaxSummer, LogSummer}

  def apply[L, L2, W](grammar: SimpleGrammar[L, L2, W], words: IndexedSeq[W]): SimpleChartMarginal[L, L2, W] = {
    SimpleChartMarginal(grammar.anchor(words), maxMarginal = false)
  }

  def apply[L, L2, W](anchoring: SimpleGrammar.Anchoring[L, L2, W], maxMarginal: Boolean): SimpleChartMarginal[L, L2, W] = {
    val sum = if (maxMarginal) MaxSummer else LogSummer
    val inside = buildInsideChart(anchoring, sum)
    val outside = buildOutsideChart(anchoring, inside, sum)
    SimpleChartMarginal(anchoring, inside, outside, maxMarginal)
  }

  private def buildInsideChart[L, L2, W](anchoring: SimpleGrammar.Anchoring[L, L2, W], sum: Summer):SimpleParseChart[L2] = {
    import anchoring._
    val length = anchoring.words.length
    val chart = new SimpleParseChart[L2](anchoring.grammar.refinedTopology.labelIndex, length)

    val lexLoc = anchoring.lexicon.anchor(anchoring.words)
    // handle lexical
    for (i <- 0 until length) {
      var visitedSomething  = false
      for {
        a <- lexLoc.allowedTags(i)
        ref <- anchoring.validLabelRefinements(i, i+ 1, a)
      } {
        val aa = anchoring.refinements.labels.globalize(a, ref)
        val score:Double = anchoring.scoreSpan(i, i+1, a, ref)
        if (score != Double.NegativeInfinity) {
          chart.bot.enter(i, i + 1, aa, score)
          visitedSomething = true
        }
      }

      updateInsideUnaries(chart, anchoring,  i, i+1, sum)
    }

    val tensor = grammar.insideTensor
    val numSyms = tensor.numLeftChildren

    for {
      span <- 2 to length
      begin <- 0 to (length - span)
    } {
      val end = begin + span
      val pcell = chart.bot.cell(begin, end)
      val pdata = pcell.data
      val pdoff = pcell.offset
      var split = begin + 1
      while (split < end) {

        val lcell = chart.top.cell(begin, split)
        val ldata = lcell.data
        val ldoff = lcell.offset
        val rcell = chart.top.cell(split, end)
        val rdata = rcell.data
        val rdoff = rcell.offset

        var lc = 0
        while (lc < numSyms) {
          val lcSpan = tensor.leftChildRange(lc)
          var rcOff = lcSpan.begin
          val rcEnd = lcSpan.end
          val bInside = ldata(ldoff + lc)
          if (bInside != Double.NegativeInfinity) {
            while (rcOff < rcEnd) {
              val rc = tensor.rightChildForOffset(rcOff)
              val cInside = rdata(rdoff + rc)

              val rcSpan = tensor.rightChildRange(rcOff)
              val withoutRule = bInside + cInside

              if (cInside != Double.NegativeInfinity) {
                var pOff = rcSpan.begin
                val pEnd = rcSpan.end
                while (pOff < pEnd) {
                  val p = tensor.parentForOffset(pOff)
                  val score = tensor.ruleScoreForOffset(pOff) + withoutRule
                  pdata(p + pdoff) = sum(pdata(p + pdoff), score)

                  pOff += 1
                }

              }

              rcOff += 1
            }
          }

          lc += 1

        }

        split += 1
      }

      updateInsideUnaries(chart, anchoring,  begin, end, sum)
    }

    chart
  }

  private def buildOutsideChart[L, L2, W](anchoring: SimpleGrammar.Anchoring[L, L2, W],
                                      inside: SimpleParseChart[L2], sum: Summer):SimpleParseChart[L2] = {
    val length = inside.length
    val refinedTopology = anchoring.refinedTopology
    val outside = new SimpleParseChart[L2](refinedTopology.labelIndex, length)
    outside.top.enter(0, inside.length, refinedTopology.rootIndex, 0.0)

    val tensor = anchoring.grammar.outsideTensor
    val numSyms = tensor.numLeftChildren

    for {
      span <- length until 0 by (-1)
      begin <- 0 to (length-span)
    } {
      val end = begin + span
      updateOutsideUnaries(outside, anchoring, begin, end, sum)

      val pcell = outside.bot.cell(begin, end)
      val pdata = pcell.data
      val pdoff = pcell.offset

      var a = 0
      while (a < numSyms) {
        val outsideA = pdata(pdoff + a)
        if (outsideA != Double.NegativeInfinity) {
          val pSpan = tensor.leftChildRange(a)
          val lcEnd = pSpan.end
          var split = begin + 1
          while (split < end) {

            val lcell = inside.top.cell(begin, split)
            val ldata = lcell.data
            val ldoff = lcell.offset
            val rcell = inside.top.cell(split, end)
            val rdata = rcell.data
            val rdoff = rcell.offset

            val olcell = outside.top.cell(begin, split)
            val oldata = olcell.data
            val oldoff = olcell.offset
            val orcell = outside.top.cell(split, end)
            val ordata = orcell.data
            val ordoff = orcell.offset

            var lcOff = pSpan.begin
            while (lcOff < lcEnd) {
              val lc = tensor.rightChildForOffset(lcOff)
              val bInside = ldata(ldoff + lc)
              if (bInside != Double.NegativeInfinity) {
                val lcSpan = tensor.rightChildRange(lcOff)
                var rcOff = lcSpan.begin
                val rcEnd = lcSpan.end

                while (rcOff < rcEnd) {
                  val rc = tensor.parentForOffset(rcOff)
                  val score = tensor.ruleScoreForOffset(rcOff) + outsideA
                  val cInside = rdata(rdoff + rc)
                  if (cInside != Double.NegativeInfinity) {
                    oldata(oldoff + lc) = sum(oldata(oldoff + lc), cInside + score)
                    ordata(ordoff + rc) = sum(ordata(ordoff + rc), bInside + score)
                    // outside.top.enter(begin, split, lc, sum(outside.top.labelScore(begin, split, lc), cInside + score))
                    // outside.top.enter(split, end, rc, sum(outside.top.labelScore(split, end, rc), bInside + score))
                  }
                  rcOff += 1
                }
              }
              lcOff += 1
            }
            split += 1
          }
        }
        a += 1
      }
    }
    outside
  }

  private def updateInsideUnaries[L, L2, W](chart: SimpleParseChart[L2],
                                        anchoring: SimpleGrammar.Anchoring[L, L2, W],
                                        begin: Int, end: Int, sum: Summer) = {
    val childCell = chart.bot.cell(begin, end)
    val parentCell = chart.top.cell(begin, end)
    val tensor = anchoring.grammar.insideTensor
    doMatrixMultiply(childCell, parentCell, tensor, sum)
  }

  private def doMatrixMultiply[W, L2, L](childCell: DenseVector[Double], parentCell: DenseVector[Double], tensor: SparseRuleTensor[L2], sum: RefinedChartMarginal.Summer) {
    val numSyms = childCell.size
    val cdata = childCell.data
    val coffset = childCell.offset
    val pdata = parentCell.data
    val poffset = parentCell.offset
    var b = 0
    while (b < numSyms) {
      val bScore = cdata(coffset + b)
      val aSpan = tensor.unaryChildRange(b)
      if (bScore != Double.NegativeInfinity) {
        var aOff = aSpan.begin
        val aEnd = aSpan.end
        while (aOff < aEnd) {
          val a = tensor.unaryParentForOffset(aOff)
          val ruleScore: Double = tensor.unaryScoreForOffset(aOff)
          val prob = pdata(a + poffset)
          pdata(a + poffset) = sum(prob, ruleScore + bScore)
          aOff += 1
        }
      }
      b += 1
    }
  }

  private def updateOutsideUnaries[L, L2, W](outside: SimpleParseChart[L2],
                                            anchoring: SimpleGrammar.Anchoring[L, L2, W],
                                            begin: Int, end: Int, sum: Summer) = {
    val childCell = outside.bot.cell(begin, end)
    val parentCell = outside.top.cell(begin, end)
    val tensor = anchoring.grammar.outsideTensor
    doMatrixMultiply(parentCell, childCell, tensor, sum)
  }

  case class SimpleChartFactory[L, L2, W](refinedGrammar: SimpleGrammar[L, L2, W], maxMarginal: Boolean = false) extends ParseMarginal.Factory[L, W] {
    def apply(w: IndexedSeq[W], constraints: ChartConstraints[L]):SimpleChartMarginal[L, L2, W] = {
      SimpleChartMarginal(refinedGrammar.anchor(w, constraints), maxMarginal = maxMarginal)
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
    @inline def cell(begin: Int, end: Int) = {
      val ind = TriangularArray.index(begin, end)
      new DenseVector[Double](scores.data, scores.offset + scores.rows * ind, 1, scores.rows)
      //scores(::, TriangularArray.index(begin, end))
    }

    def apply(begin: Int, end: Int, label: L):Double = apply(begin, end, index(label))
    def labelScore(begin: Int, end: Int, label: L):Double = apply(begin, end, index(label))

    def enter(begin: Int, end: Int, parent: Int, w: Double): Unit = {
      scores(parent, TriangularArray.index(begin, end)) = w
    }

  }

}
