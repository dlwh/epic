package scalanlp.parser
package lex

import ParseChart._

import math.exp
import scalala.tensor.mutable.{Counter, Counter2, Vector}

import LexInsideOutside._
import scalanlp.tensor.sparse.OldSparseVector
import scalala.tensor.dense.{DenseVectorCol, DenseVector}

/**
 * InsideOutside computes expected counts for rules and lexical emissions for a chart builder
 * @author dlwh
 */
class LexInsideOutside[L,W](featurizer: IndexedFeaturizer[L,W],
                            val parser: LexChartBuilder[LogProbabilityParseChart,L,W]) {
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
    val vector = DenseVector.zeros[Double](featurizer.featureIndex.size)
    computeWordCounts(spec, inside, outside, validSpan, totalProb, vector)
    computeRuleCounts(spec, inside, outside, validSpan, totalProb, vector)

    ExpectedCounts[W](vector, totalProb)
  }

  private def computeWordCounts(spec: LexGrammar[L,W]#Specialization,
                                inside: LogProbabilityParseChart[L],
                                outside: LogProbabilityParseChart[L],
                                validSpan: SpanScorer[L],
                                totalProb: Double,
                                vector: DenseVector[Double]) {
    val fspec = featurizer.specialize(spec.words)
    for (i <- 0 until spec.words.length) {
      for (lh <- inside.bot.enteredLabelIndexes(i, i + 1)) {
        val l = inside.bot.decodeLabelPart(lh)
        if (isTag(l)) {
          val iScore = inside.bot.labelScore(i, i + 1, lh)
          val oScore = outside.bot.labelScore(i, i + 1, lh)
          val count = exp(iScore + oScore - totalProb)
          if(count > 0)
            addMultiple(vector, fspec.featuresForTag(l,i), count)
        }
      }
    }
  }


  private def computeRuleCounts(spec: LexGrammar[L,W]#Specialization,
                                inside: LogProbabilityParseChart[L],
                                outside: LogProbabilityParseChart[L],
                                validSpan: SpanScorer[L],
                                totalProb: Double,
                                vector: DenseVector[Double]) = {
    val fspec = featurizer.specialize(spec.words)
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
                addMultiple(vector, fspec.featuresForBinary(r,h,right), selfScore)
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
                if(selfScore >= 0)
                  addMultiple(vector, fspec.featuresForBinary(r,h,left), selfScore)
              }
            }
        }

      }
    }

    // Unaries
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
        if(prob >= 0)
          addMultiple(vector, fspec.featuresForUnary(r,h), prob)
      }
    }
  }

  private val isTag = grammar.indexedTags
}

object LexInsideOutside {

  final case class ExpectedCounts[W](features: DenseVectorCol[Double],
                                     var logProb: Double) extends scalanlp.parser.epic.ExpectedCounts[ExpectedCounts[W]] {

    def loss = logProb

    def this(numFeatures: Int) = this(DenseVector.zeros(numFeatures), 0.0)

//    def decode[L](g: Grammar[L]) = (decodeRules(g,ruleCounts), decodeWords(g,wordCounts))

    def +=(c: ExpectedCounts[W]) = {
      features += c.features
      logProb += c.logProb
      this
    }

    def -=(c: ExpectedCounts[W]) = {
      features -= c.features
      logProb -= c.logProb
      this
    }


  }


  private def addMultiple(vec: DenseVector[Double], feats: Array[Int], d: Double) = {
    var i = 0
    while(i < feats.length) {
      vec(feats(i)) += d
      i += 1
    }
    vec
  }


}