package scalanlp.parser

import ParseChart._

import InsideOutside._

import math.exp
import scalala.tensor.::
import scalala.tensor.mutable.{Counter, Counter2, Vector}
import scalala.tensor.dense.DenseVector
import scalanlp.trees.{BinaryRule, UnaryRule}

/**
 * InsideOutside computes expected counts for rules and lexical emissions for a chart builder
 * @author dlwh
 */
class InsideOutside[L,W](val parser: ChartBuilder[LogProbabilityParseChart,L,W]) {
  def this(root: L, g: Grammar[L], lexicon: Lexicon[L,W])  = {
    this(new CKYChartBuilder[ParseChart.LogProbabilityParseChart,L,W](root,lexicon,g,logProb))
  }

  def grammar = parser.grammar
  def lexicon = parser.lexicon
  def root = parser.root

  def expectedCounts(words: Seq[W], validSpan: SpanScorer[L] = SpanScorer.identity,
                     spanVisitor: AnchoredSpanVisitor = AnchoredSpanVisitor.noOp):ExpectedCounts[W] = {
    val inside = parser.buildInsideChart(words, validSpan)
    val totalProb = inside.top.labelScore(0, words.length, root)
    val outside = parser.buildOutsideChart(inside, validSpan)

    expectedCounts(words,inside,outside, totalProb, validSpan, spanVisitor)
  }

  def expectedCounts(words: Seq[W],
                     inside: LogProbabilityParseChart[L],
                     outside: LogProbabilityParseChart[L],
                     totalProb: Double, validSpan: SpanScorer[L],
                     spanVisitor: AnchoredSpanVisitor) = {
    val wordCounts = computeWordCounts(words, inside, outside, validSpan, totalProb, spanVisitor)
    val ruleCounts = computeRuleCounts(words, inside, outside, validSpan, totalProb, spanVisitor)

    ExpectedCounts(ruleCounts, wordCounts, totalProb)
  }

  private def computeWordCounts(words: scala.Seq[W],
                                inside: LogProbabilityParseChart[L],
                                outside: LogProbabilityParseChart[L],
                                validSpan: SpanScorer[L],
                                totalProb: Double,
                                spanVisitor: AnchoredSpanVisitor): Array[Counter[W, Double]] = {
    val wordCounts = grammar.labelEncoder.fillArray(Counter[W, Double]())
    // handle lexical productions:
    for (i <- 0 until words.length) {
      val w = words(i)
      for (l <- inside.bot.enteredLabelIndexes(i, i + 1) if isTag(l)) {
        val iScore = inside.bot.labelScore(i, i + 1, l)
        val oScore = outside.bot.labelScore(i, i + 1, l)
        val count = exp(iScore + oScore - totalProb)
        if(spanVisitor ne AnchoredSpanVisitor.noOp) spanVisitor.visitSpan(i,i+1,l,count)
        wordCounts(l)(w) += count
      }
    }
    wordCounts
  }

  private def computeRuleCounts(words: scala.Seq[W],
                                  inside: LogProbabilityParseChart[L],
                                  outside: LogProbabilityParseChart[L],
                                  validSpan: SpanScorer[L],
                                  totalProb: Double,
                                  spanVisitor: AnchoredSpanVisitor = AnchoredSpanVisitor.noOp) = {
    val ruleCounts = grammar.mkDenseVector()
    val data = ruleCounts.data
    // handle binary rules
    for{
      span <- 2 to words.length
      begin <- 0 to (words.length - span)
    } {
      val end = begin + span

      val narrowRight = inside.top.narrowRight(begin)
      val narrowLeft = inside.top.narrowLeft(end)
      val wideRight = inside.top.wideRight(begin)
      val wideLeft = inside.top.wideLeft(end)

      for(a <- inside.bot.enteredLabelIndexes(begin,end)) {
        var i = 0;
        val rules = grammar.indexedBinaryRulesWithParent(a)
        val spanScore = validSpan.scoreSpan(begin,end,a)
        while(i < rules.length) {
          val r = rules(i)
          val b = grammar.leftChild(r)
          val c = grammar.rightChild(r)
          val ruleScore = grammar.ruleScore(r)
          i += 1

          // this is too slow, so i'm having to inline it.
          //              val feasibleSpan = itop.feasibleSpanX(begin, end, b, c)
          val narrowR = narrowRight(b)
          val narrowL = narrowLeft(c)

          val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
            0L
          } else {
            val trueX = wideLeft(c)
            val trueMin = if(narrowR > trueX) narrowR else trueX
            val wr = wideRight(b)
            val trueMax = if(wr < narrowL) wr else narrowL
            if(trueMin > narrowL || trueMin > trueMax)  0L
            else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
          }

          var split = (feasibleSpan >> 32).toInt
          val endSplit = feasibleSpan.toInt // lower 32 bits
          var selfScore = 0.0
          while(split < endSplit) {
            val bScore = inside.top.labelScore(begin, split, b)
            val cScore = inside.top.labelScore(split, end, c)
            val aScore = outside.bot.labelScore(begin, end, a)
            val rScore = ruleScore + validSpan.scoreBinaryRule(begin,split,end,r) + spanScore
            val prob = exp(bScore + cScore + aScore + rScore - totalProb)
            if(spanVisitor ne AnchoredSpanVisitor.noOp) spanVisitor.visitBinaryRule(begin,split,end,r,prob)
            if(prob != 0.0) {
              selfScore += prob
            }
            split += 1
          }
          data(r) += selfScore
          if(spanVisitor ne AnchoredSpanVisitor.noOp) spanVisitor.visitSpan(begin,end,a,selfScore)
        }
      }
    }

    // Unaries
    // TODO: only iterate over observed counts
    for{
      span <- 1 to words.length
      begin <- 0 to (words.length - span)
      end = begin + span
      a <- inside.top.enteredLabelIndexes(begin,end)
      r <- grammar.indexedUnaryRulesWithParent(a)
    } {
      val b = grammar.child(r)
      val bScore = inside.bot.labelScore(begin, end, b)
      val aScore = outside.top.labelScore(begin, end, a)
      val rScore = grammar.ruleScore(r) + validSpan.scoreUnaryRule(begin,end,r);
      val prob = exp(bScore + aScore + rScore - totalProb);
      if(prob != 0.0) {
        data(r) += prob
        if(spanVisitor ne AnchoredSpanVisitor.noOp) spanVisitor.visitUnaryRule(begin,end,r,prob)
      }
    }
    ruleCounts
  }

  private val isTag = new collection.mutable.BitSet()
  lexicon.tags.foreach {l => isTag += grammar.labelIndex(l)}
}

object InsideOutside {

  final case class ExpectedCounts[W](ruleCounts: DenseVector[Double],
                                     wordCounts: Array[Counter[W,Double]], // parent -> word -> counts
                                     var logProb: Double) {

    def this(g: Grammar[_]) = this(g.mkDenseVector(),
      g.labelEncoder.fillArray(Counter[W,Double]()), 0.0)

    def this(numRules: Int, numLabels: Int) = this(DenseVector.zeros[Double](numRules),
      Array.fill(numLabels)(Counter[W,Double]()), 0.0)

    def decode[L](g: Grammar[L]) = (decodeRules(g,ruleCounts), decodeWords(g,wordCounts))

    def +=(c: ExpectedCounts[W]) = {
      val ExpectedCounts(bCounts,wCounts,tProb) = c

      this.ruleCounts += c.ruleCounts

      for( (vec, k) <- wCounts.iterator.zipWithIndex) {
        wordCounts(k) += vec
      }

      logProb += tProb
      this
    }

    def -=(c: ExpectedCounts[W]) = {
      val ExpectedCounts(bCounts,wCounts,tProb) = c

      this.ruleCounts -= c.ruleCounts

      for( (vec,k) <- wCounts.iterator.zipWithIndex) {
        wordCounts(k) -= vec
      }

      logProb -= tProb
      this
    }


  }

  def decodeRules[L](g: Grammar[L],
                     ruleCounts: Vector[Double]) = {
    val binaries = Counter2[L,BinaryRule[L],Double]()
    val unaries = Counter2[L,UnaryRule[L],Double]()

    for ( (r,score) <- ruleCounts.pairsIteratorNonZero) {
      val rule = g.index.get(r)
      rule match {
        case rule@BinaryRule(p,_,_) =>
          binaries(p,rule) = score
        case rule@UnaryRule(p,c) =>
          unaries(p,rule) = score
      }
    }
    (binaries,unaries)
  }

  def decodeWords[L,W](g: Grammar[L], wordCounts: Array[Counter[W,Double]]) = {
    val ctr = Counter2[L,W,Double]()
    for( (c,i) <- wordCounts.zipWithIndex) {
      ctr(g.labelIndex.get(i), ::) := c
    }
    ctr
  }



}