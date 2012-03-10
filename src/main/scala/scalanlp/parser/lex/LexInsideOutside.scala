package scalanlp.parser
package lex

import ParseChart._

import math.exp
import scalala.tensor.mutable.{Counter, Counter2, Vector}

import LexInsideOutside._

/**
 * InsideOutside computes expected counts for rules and lexical emissions for a chart builder
 * @author dlwh
 */
class LexInsideOutside[L,W](val parser: LexChartBuilder[LogProbabilityParseChart,L,W]) {
  def grammar = parser.grammar
  def root = parser.root
  val indexedRoot = grammar.labelIndex(root)

  def expectedCounts(words: Seq[W], validSpan: SpanScorer[L] = SpanScorer.identity):ExpectedCounts[W] = {
    val pair = parser.buildCharts(words,validSpan)
    import pair._

    expectedCounts(spec, inside, outside, pair.partition, validSpan)
  }

  def expectedCounts(spec: LexGrammar[L,W]#Specialization,
                     inside: LogProbabilityParseChart[L],
                     outside: LogProbabilityParseChart[L],
                     totalProb: Double, validSpan: SpanScorer[L]) = {
    val wordCounts = computeWordCounts(spec, inside, outside, validSpan, totalProb)
    val (lc,uc) = computeRuleCounts(spec, inside, outside, validSpan, totalProb)

    ExpectedCounts(lc, uc, wordCounts, totalProb)
  }

  private def computeWordCounts(spec: LexGrammar[L,W]#Specialization,
                                inside: LogProbabilityParseChart[L],
                                outside: LogProbabilityParseChart[L],
                                validSpan: SpanScorer[L],
                                totalProb: Double): Array[Counter[W, Double]] = {
    val wordCounts = grammar.labelEncoder.fillArray(Counter[W, Double]())
    // handle lexical productions:
    for (i <- 0 until spec.words.length) {
      val w = spec.words(i)
      for (lh <- inside.bot.enteredLabelIndexes(i, i + 1)) {
        val l = inside.bot.decodeLabelPart(lh)
        if (isTag(l)) {
          val iScore = inside.bot.labelScore(i, i + 1, lh)
          val oScore = outside.bot.labelScore(i, i + 1, lh)
          val count = exp(iScore + oScore - totalProb)
          wordCounts(l)(w) += count
        }
      }
    }
    wordCounts
  }

  private def computeRuleCounts(spec: LexGrammar[L,W]#Specialization,
                                inside: LogProbabilityParseChart[L],
                                outside: LogProbabilityParseChart[L],
                                validSpan: SpanScorer[L],
                                totalProb: Double) = {
    val lCounts = Array.fill(grammar.index.size)(Counter2[W,W,Double]())
    val uCounts = Array.fill(grammar.index.size)(Counter[W,Double]())
    // handle binary rules
    for{
      span <- 2 to inside.length
      begin <- 0 to (inside.length - span)
    } {
      val end = begin + span

      val narrowRight = inside.top.narrowRight(begin)
      val narrowLeft = inside.top.narrowLeft(end)
      val wideRight = inside.top.wideRight(begin)
      val wideLeft = inside.top.wideLeft(end)

      for(ah <- inside.bot.enteredLabelIndexes(begin,end)) {
        val a = outside.bot.decodeLabelPart(ah)
        val h = outside.bot.decodeHeadPart(ah)
        var i = 0;
        val rules = grammar.indexedBinaryRulesWithParent(a)
        val spanScore = validSpan.scoreSpan(begin,end,a)
        val aScore = outside.bot.labelScore(begin, end, ah)
        if(!aScore.isInfinite)
        while(i < rules.length) {
          val r = rules(i)
          val b = grammar.leftChild(r)
          val c = grammar.rightChild(r)
          i += 1
          if(grammar.isLeftRule(r))
            for(right <- (h + 1) until end) {

              // this is too slow, so i'm having to inline it.
              //              val feasibleSpan = itop.feasibleSpanX(begin, end, b, c)
              val narrowR = narrowRight(inside.top.encodeLabelPair(b, h))
              val narrowL = narrowLeft(inside.top.encodeLabelPair(c, right))

              val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                0L
              } else {
                val trueX = wideLeft(inside.top.encodeLabelPair(c, right))
                val trueMin = if(narrowR > trueX) narrowR else trueX
                val wr = wideRight(inside.top.encodeLabelPair(b, h))
                val trueMax = if(wr < narrowL) wr else narrowL
                if(trueMin > narrowL || trueMin > trueMax)  0L
                else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
              }

              var split = (feasibleSpan >> 32).toInt
              val endSplit = feasibleSpan.toInt // lower 32 bits
              if(split < endSplit) {
                val ruleScore = spec.scoreRightComplement(r, h, right)
                var selfScore = 0.0
                while(split < endSplit) {
                  val bScore = inside.top.labelScore(begin, split, b, h)
                  val cScore = inside.top.labelScore(split, end, c, right)
                  val rScore = ruleScore + validSpan.scoreBinaryRule(begin,split,end,r) + spanScore
                  val prob = exp(bScore + cScore + aScore + rScore - totalProb)
                  if(prob != 0.0) {
                    selfScore += prob
                  }
                  split += 1
                }
                val wH = spec.words(h)
                val wR = spec.words(right)
                lCounts(r)(wH,wR) += selfScore
              }
            }

          if(grammar.isRightRule(r))
            for(left <- begin until h) {
              val narrowR = narrowRight(inside.top.encodeLabelPair(b, left))
              val narrowL = narrowLeft(inside.top.encodeLabelPair(c, h))

              val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                0L
              } else {
                val trueX = wideLeft(inside.top.encodeLabelPair(c, h))
                val trueMin = if(narrowR > trueX) narrowR else trueX
                val wr = wideRight(inside.top.encodeLabelPair(b, left))
                val trueMax = if(wr < narrowL) wr else narrowL
                if(trueMin > narrowL || trueMin > trueMax)  0L
                else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
              }
              var split = (feasibleSpan >> 32).toInt
              val endSplit = feasibleSpan.toInt // lower 32 bits

              if (split < endSplit) {
                val ruleScore = spec.scoreLeftComplement(r, h, left)
                var selfScore = 0.0
                while(split < endSplit) {
                  val bScore = inside.top.labelScore(begin, split, b, left)
                  val cScore = inside.top.labelScore(split, end, c, h)
                  val rScore = ruleScore + validSpan.scoreBinaryRule(begin,split,end,r) + spanScore
                  val prob = exp(bScore + cScore + aScore + rScore - totalProb)
                  if(prob != 0.0) {
                    selfScore += prob
                  }
                  split += 1
                }
                val wH = spec.words(h)
                val wR = spec.words(left)
                lCounts(r)(wH,wR) += selfScore
              }
            }
        }

      }
    }

    // Unaries
    // TODO: only iterate over observed counts
    for{
      span <- 1 to spec.words.length
      begin <- 0 to (spec.words.length - span)
      end = begin + span
      ah <- inside.top.enteredLabelIndexes(begin,end)
    } {
      val a = outside.bot.decodeLabelPart(ah)
      val h = outside.bot.decodeHeadPart(ah)
      for(r <- grammar.indexedUnaryRulesWithParent(a)) {
        val b = grammar.child(r)
        val bScore = inside.bot.labelScore(begin, end, b, h)
        val aScore = outside.top.labelScore(begin, end, a, h)
        val rScore = spec.scoreUnary(r, h) + validSpan.scoreUnaryRule(begin,end,r);
        val prob = exp(bScore + aScore + rScore - totalProb);
        if(prob != 0.0) {
          uCounts(r)(spec.words(h)) += prob
        }
      }
    }
    (lCounts,uCounts)
  }

  private val isTag = grammar.indexedTags
}

object LexInsideOutside {

  final case class ExpectedCounts[W](bCounts: Array[Counter2[W,W,Double]],
                                     unaryCounts: Array[Counter[W,Double]],
                                     wordCounts: Array[Counter[W,Double]], // parent -> word -> counts
                                     var logProb: Double) extends scalanlp.parser.epic.ExpectedCounts[ExpectedCounts[W]] {

    def loss = logProb

    def this(numRules: Int, numLabels: Int) = this(Array.fill(numRules)(Counter2[W,W,Double]()),
      Array.fill(numRules)(Counter[W,Double]()),
      Array.fill(numLabels)(Counter[W,Double]()), 0.0)

//    def decode[L](g: Grammar[L]) = (decodeRules(g,ruleCounts), decodeWords(g,wordCounts))

    def +=(c: ExpectedCounts[W]) = {

      for( (a,b) <- bCounts zip c.bCounts) {
        a += b
      }

      for( (a,b) <- unaryCounts zip c.unaryCounts) {
        a += b
      }

      for( (vec, k) <- c.wordCounts.iterator.zipWithIndex) {
        wordCounts(k) += vec
      }

      logProb += c.logProb
      this
    }

    def -=(c: ExpectedCounts[W]) = {

      for( (a,b) <- bCounts zip c.bCounts) {
        a -= b
      }

      for( (a,b) <- unaryCounts zip c.unaryCounts) {
        a -= b
      }

      for( (vec,k) <- c.wordCounts.iterator.zipWithIndex) {
        wordCounts(k) -= vec
      }

      logProb -= c.logProb
      this
    }


  }

//  def decodeRules[L](g: Grammar[L],
//                     ruleCounts: Vector[Double]) = {
//    val binaries = Counter2[L,BinaryRule[L],Double]()
//    val unaries = Counter2[L,UnaryRule[L],Double]()
//
//    for ( (r,score) <- ruleCounts.pairsIteratorNonZero) {
//      val rule = g.index.get(r)
//      rule match {
//        case rule@BinaryRule(p,_,_) =>
//          binaries(p,rule) = score
//        case rule@UnaryRule(p,c) =>
//          unaries(p,rule) = score
//      }
//    }
//    (binaries,unaries)
//  }

//  def decodeWords[L,W](g: Grammar[L], wordCounts: Array[Counter[W,Double]]) = {
//    val ctr = Counter2[L,W,Double]()
//    for( (c,i) <- wordCounts.zipWithIndex) {
//      ctr(g.labelIndex.get(i), ::) := c
//    }
//    ctr
//  }



}