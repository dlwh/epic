package scalanlp.parser

import scalanlp.collection.mutable.SparseArrayMap
import ParseChart._

import InsideOutside._

import math.exp
import scalala.tensor.::
import scalala.tensor.mutable.{Counter, Counter2, Vector}

/**
 * 
 * @author dlwh
 */
class InsideOutside[L,W](val parser: ChartBuilder[LogProbabilityParseChart,L,W]) {
  def this(root: L, g: Grammar[L], lexicon: Lexicon[L,W])  = {
    this(new CKYChartBuilder[ParseChart.LogProbabilityParseChart,L,W](root,lexicon,g,logProb))
  }

  def grammar = parser.grammar
  def lexicon = parser.lexicon
  def root = parser.root

  def expectedCounts(words: Seq[W], validSpan: SpanScorer[L] =SpanScorer.identity):ExpectedCounts[W] = {
    val inside = parser.buildInsideChart(words, validSpan)
    val totalProb = inside.top.labelScore(0, words.length, root)
    val outside = parser.buildOutsideChart(inside, validSpan)

    expectedCounts(words,inside,outside, totalProb, validSpan)
  }

  def expectedCounts(words: Seq[W],
                     inside: LogProbabilityParseChart[L],
                     outside: LogProbabilityParseChart[L],
                     totalProb: Double, validSpan: SpanScorer[L]) = {
    val wordCounts = computeWordCounts(words, inside, outside, validSpan, totalProb)
    val ruleCounts = computeBinaryCounts(words, inside, outside, validSpan, totalProb)
    val unaryRuleCounts = computeUnaryCounts(words, inside, outside, validSpan, totalProb)

    ExpectedCounts(ruleCounts + unaryRuleCounts, wordCounts, totalProb)
  }

  private def computeWordCounts(words: scala.Seq[W],
                                inside: LogProbabilityParseChart[L],
                                outside: LogProbabilityParseChart[L],
                                validSpan: SpanScorer[L],
                                totalProb: Double): SparseArrayMap[Counter[W, Double]] = {
    val wordCounts = grammar.labelEncoder.fillSparseArrayMap(Counter[W, Double]())
    // handle lexical productions:
    for (i <- 0 until words.length) {
      val w = words(i)
      for (l <- inside.bot.enteredLabelIndexes(i, i + 1) if isTag(l)) {
        val iScore = inside.bot.labelScore(i, i + 1, l)
        val oScore = outside.bot.labelScore(i, i + 1, l)
        wordCounts.getOrElseUpdate(l)(w) += exp(iScore + oScore - totalProb)
      }
    }
    wordCounts
  }

  private def computeBinaryCounts(words: scala.Seq[W],
                                  inside: LogProbabilityParseChart[L],
                                  outside: LogProbabilityParseChart[L],
                                  validSpan: SpanScorer[L], totalProb: Double) = {
    val ruleCounts = grammar.mkDenseVector()
    // handle binary rules
    for{
      span <- 2 to words.length
      begin <- 0 to (words.length - span)
      end = begin + span
      a <- inside.bot.enteredLabelIndexes(begin,end)
      r <- grammar.indexedBinaryRulesWithParent(a)
      b = grammar.leftChild(r)
      c = grammar.rightChild(r)
      split <- inside.top.feasibleSpan(begin, end, b, c)
    } {
      val bScore = inside.top.labelScore(begin, split, b)
      val cScore = inside.top.labelScore(split, end, c)
      val aScore = outside.bot.labelScore(begin, end, a)
      val rScore = grammar.ruleScore(r) + validSpan.scoreBinaryRule(begin,split,end,r) + validSpan.scoreSpan(begin,split,a)
      val prob = exp(bScore + cScore + aScore + rScore - totalProb)
      if(prob != 0.0)
        ruleCounts(r) += prob
    }
    ruleCounts
  }

  private def computeUnaryCounts(words: scala.Seq[W],
                                 inside: LogProbabilityParseChart[L],
                                 outside: LogProbabilityParseChart[L],
                                 validSpan: SpanScorer[L],
                                 totalProb: Double) = {
    val ruleCounts = grammar.mkDenseVector()
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
      if(prob != 0.0)
        ruleCounts(r) += prob
    }
    ruleCounts
  }

  private val isTag = new collection.mutable.BitSet()
  lexicon.tags.foreach {l => isTag += grammar.labelIndex(l)}
}

object InsideOutside {

  final case class ExpectedCounts[W](
    ruleCounts: Vector[Double],
    wordCounts: SparseArrayMap[Counter[W,Double]], // parent -> word -> counts
    var logProb: Double
  ) {

    def this(g: Grammar[_]) = this(g.mkOldSparseVector(),
                                   g.labelEncoder.fillSparseArrayMap(Counter[W,Double]()), 0.0)

    def decode[L](g: Grammar[L]) = (decodeRules(g,ruleCounts), decodeWords(g,wordCounts))

    def +=(c: ExpectedCounts[W]) = {
      val ExpectedCounts(bCounts,wCounts,tProb) = c

      this.ruleCounts += c.ruleCounts

      for( (k,vec) <- wCounts) {
        wordCounts.getOrElseUpdate(k) += vec
      }

      logProb += tProb
      this
    }

    def -=(c: ExpectedCounts[W]) = {
      val ExpectedCounts(bCounts,wCounts,tProb) = c

      this.ruleCounts -= c.ruleCounts

      for( (k,vec) <- wCounts) {
        wordCounts.getOrElseUpdate(k) -= vec
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

  def decodeWords[L,W](g: Grammar[L], wordCounts: SparseArrayMap[Counter[W,Double]]) = {
    val ctr = Counter2[L,W,Double]()
    for( (i,c) <- wordCounts) {
      ctr(g.labelIndex.get(i), ::) := c
    }
    ctr
  }



}