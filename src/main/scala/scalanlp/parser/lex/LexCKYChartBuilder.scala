package scalanlp.parser.lex

import scalanlp.parser.ParseChart.Factory
import scalala.library.Numerics
import scalanlp.parser.{SpanScorer, ParseChart}


case class LexChartPair[+PC[X]<:ParseChart[X],L, W](spec: LexGrammar[L,W]#Specialization,
                                                    inside: PC[L],
                                                    outside: PC[L],
                                                    partition: Double,
                                                    scorer: SpanScorer[L] = SpanScorer.identity)

/**
 * Trait for things that parse. So far, we only have CKYChartBuilder
 */
trait LexChartBuilder[+Chart[X]<:ParseChart[X], L, W] extends Serializable {
  /**
   * Given a sentence s, fills a parse chart with inside scores.
   */
  def buildInsideChart(s: Seq[W], validSpan: SpanScorer[L] = SpanScorer.identity):Chart[L]

  def buildCharts(s: Seq[W], validSpan: SpanScorer[L]):LexChartPair[Chart, L, W]

  def rootScore(inside: ParseChart[L]) = {
    val indexedRoot = grammar.labelIndex(root)
    val rootScores = new Array[Double](inside.length)
    var offset = 0
    for(i <- 0 until inside.length) {
      val score = inside.top.labelScore(0, inside.length, indexedRoot, i)
      if(score != Double.NegativeInfinity) {
        rootScores(offset) = score
        offset += 1
      }
    }
    inside.sum(rootScores,offset)
  }

  def root: L
  val grammar: LexGrammar[L, W]

  def index = grammar.labelIndex

  /**
   * Change the kind of parse chart that gets built
   */
  def withCharts[Chart[X]<:ParseChart[X]](factory: ParseChart.Factory[Chart]):LexChartBuilder[Chart, L, W]

  def chartFactory: Factory[Chart]
}

/**
 * 
 * @author dlwh
 */

class LexCKYChartBuilder[Chart[X]<:ParseChart[X], L, W](val root: L,
                                                        val grammar: LexGrammar[L, W],
                                                        val chartFactory: Factory[Chart]) extends LexChartBuilder[Chart, L, W] {
  def withCharts[Chart[X] <: ParseChart[X]](factory: Factory[Chart]):LexChartBuilder[Chart, L, W] = {
    new LexCKYChartBuilder[Chart, L, W](root, grammar, factory)
  }


  def buildInsideChart(s: Seq[W], validSpan: SpanScorer[L]) = {
    val spec = grammar.specialize(s)
    buildInsideChart(spec,validSpan)
  }

  def buildCharts(s: Seq[W], validSpan: SpanScorer[L]) = {
    val spec = grammar.specialize(s)
    val inside =  buildInsideChart(spec,validSpan)
    val root = rootScore(inside)
    val outside =  buildOutsideChart(inside, spec, validSpan)
    LexChartPair[Chart,L,W](spec, inside, outside, root, validSpan)
  }

  def buildInsideChart(spec: grammar.Specialization, validSpan: SpanScorer[L]):Chart[L] = {
    import spec.words
    val chart = chartFactory(grammar.labelEncoder, words.length, true)

    for{i <- 0 until words.length} {
      var foundSomething = false
      for {
        (a, wScore) <- spec.tagScores(i).activeIterator if !wScore.isInfinite && !wScore.isNaN
      } {
        val spanScore:Double = validSpan.scoreSpan(i, i+1, a)
        if (spanScore != Double.NegativeInfinity) {
          chart.bot.enter(i, i+1, a, i, wScore + spanScore)
          foundSomething = true
        }
      }
      if(!foundSomething) {
        println("Warning: couldn't score "+ words(i))
        for {
          (a, wScore) <- spec.tagScores(i).activeIterator if !wScore.isInfinite && !wScore.isNaN
        } {
          chart.bot.enter(i, i+1, a, i, wScore - 10)
        }
      }

      updateInsideUnaries(chart, spec,  i, i+1, validSpan)
    }

    val maxSpanLength = words.length min 100
    // 100 = max span length
    val scoreArray = new Array[Double](grammar.maxNumBinaryRulesForParent * maxSpanLength * words.length * words.length)

    val top = chart.top

    // a -> bc over [begin, split, end)
    for {
      span <- 2 to (maxSpanLength)
      begin <- 0 to (words.length - span)
      end = begin + span
    } {
      val narrowRight = top.narrowRight(begin)
      val narrowLeft = top.narrowLeft(end)
      val wideRight = top.wideRight(begin)
      val wideLeft = top.wideLeft(end)
      for ( h <- begin until end; a <- spec.labelsForHead(h) ) {
        val passScore = validSpan.scoreSpan(begin, end, a)
        var offset = 0 // into scoreArray
        if(!passScore.isInfinite) {
          var ruleIndex = 0 // into rules
          val rules = grammar.rulesForLabel(a)
          while(ruleIndex < rules.length) {
            val r = rules(ruleIndex)
            val b = grammar.leftChild(r)
            val c = grammar.rightChild(r)
            ruleIndex += 1
            // Left child is head: extend right
            if(grammar.isLeftRule(r)) {
              var right = h+1;
              while(right < end)  {
                val narrowR = narrowRight(chart.top.encodeLabelPair(b, h))
                val narrowL = narrowLeft(chart.top.encodeLabelPair(c, right))

                val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                  0L
                } else {
                  val trueX = wideLeft(chart.top.encodeLabelPair(c, right))
                  val trueMin = if(narrowR > trueX) narrowR else trueX
                  val wr = wideRight(chart.top.encodeLabelPair(b, h))
                  val trueMax = if(wr < narrowL) wr else narrowL
                  if(trueMin > narrowL || trueMin > trueMax)  0L
                  else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
                }
                var split = (feasibleSpan >> 32).toInt
                val endSplit = feasibleSpan.toInt // lower 32 bits
                if(split < endSplit) {
                  val ruleScore = spec.scoreRightComplement(r, h, right)
                  while(split < endSplit) {
                    val bScore = chart.top.labelScore(begin, split, b, h)
                    val cScore = chart.top.labelScore(split, end, c, right)
                    val spanScore = validSpan.scoreBinaryRule(begin, split, end, r) + passScore
                    val totalA = ruleScore + spanScore
                    val prob = bScore + cScore + totalA
                    if(!java.lang.Double.isInfinite(prob)) {
                      scoreArray(offset) = prob
                      offset += 1
                    }
                    split += 1
                  }
                }
                right += 1
              }
            }

            if(grammar.isRightRule(r)) {
              var left = begin
              while(left < h) {
                val narrowR = narrowRight(chart.top.encodeLabelPair(b, left))
                val narrowL = narrowLeft(chart.top.encodeLabelPair(c, h))

                val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                  0L
                } else {
                  val trueX = wideLeft(chart.top.encodeLabelPair(c, h))
                  val trueMin = if(narrowR > trueX) narrowR else trueX
                  val wr = wideRight(chart.top.encodeLabelPair(b, left))
                  val trueMax = if(wr < narrowL) wr else narrowL
                  if(trueMin > narrowL || trueMin > trueMax)  0L
                  else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
                }
                var split = (feasibleSpan >> 32).toInt
                val endSplit = feasibleSpan.toInt // lower 32 bits
                if(split < endSplit) {
                  val ruleScore = spec.scoreLeftComplement(r, h, left)
                  while(split < endSplit) {
                    val bScore = chart.top.labelScore(begin, split, b, left)
                    val cScore = chart.top.labelScore(split, end, c, h)
                    val spanScore = validSpan.scoreBinaryRule(begin, split, end, r) + passScore
                    val totalA = ruleScore + spanScore
                    val prob = bScore + cScore + totalA
                    if(!java.lang.Double.isInfinite(prob)) {
                      scoreArray(offset) = prob
                      offset += 1
                    }
                    split += 1
                  }
                }
                left += 1
              }
            }
          }
        }
        // done updating vector, do an enter:
        if(offset > 0) {
          chart.bot.enter(begin, end, a, h, scoreArray, offset)
        }

      }
      updateInsideUnaries(chart, spec, begin, end, validSpan)
    }
    chart
  }

  private def buildOutsideChart(inside: Chart[L], spec: grammar.Specialization,
                                validSpan: SpanScorer[L] = SpanScorer.identity):Chart[L] = {
    val length = inside.length
    val outside = chartFactory(grammar.labelEncoder, length, lexicalize = true)
    for(h <- 0 until inside.length)
      outside.top.enter(0, inside.length, grammar.labelIndex(root), h, 0.0)

    val itop = inside.top
    val ibot = inside.top
    for {
      span <- length until 0 by (-1)
      begin <- 0 to (length-span)
    } {
      val end = begin + span
      val narrowRight = itop.narrowRight(begin)
      val narrowLeft = itop.narrowLeft(end)
      val wideRight = itop.wideRight(begin)
      val wideLeft = itop.wideLeft(end)
      updateOutsideUnaries(outside, inside, spec, begin, end, validSpan)
      if(span > 1)
        // a[h] ->  b[h] c[x]  [begin, split, end) | -> b[x] c[h]
        for (ah <- outside.bot.enteredLabelIndexes(begin, end) ) {
          val a = outside.bot.decodeLabelPart(ah)
          val h = outside.bot.decodeHeadPart(ah)
          val aScore = outside.bot.labelScore(begin, end, ah) + validSpan.scoreSpan(begin, end, a)
          if (!aScore.isInfinite) {
            val rules = grammar.indexedBinaryRulesWithParent(a)
            var br = 0;
            while(br < rules.length) {
              val r = rules(br)
              val b = grammar.leftChild(r)
              val c = grammar.rightChild(r)
              br += 1

              if(grammar.isLeftRule(r)) {
                var right = h+1;
                while(right < end)  {
                  val narrowR = narrowRight(itop.encodeLabelPair(b, h))
                  val narrowL = narrowLeft(itop.encodeLabelPair(c, right))

                  // this is too slow, so i'm having to inline it.
                  //              val feasibleSpan = itop.feasibleSpanX(begin, end, b, c)
                  val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                    0L
                  } else {
                    val trueX = wideLeft(itop.encodeLabelPair(c, right))
                    val trueMin = if(narrowR > trueX) narrowR else trueX
                    val wr = wideRight(itop.encodeLabelPair(b, h))
                    val trueMax = if(wr < narrowL) wr else narrowL
                    if(trueMin > narrowL || trueMin > trueMax)  0L
                    else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
                  }

                  var split = (feasibleSpan >> 32).toInt
                  val endSplit = feasibleSpan.toInt // lower 32 bits
                  if(split < endSplit) {
                    val ruleScore = spec.scoreRightComplement(r, h, right)
                    while(split < endSplit) {
                      val score = aScore + ruleScore
                      val bInside = itop.labelScore(begin, split, b, h)
                      val cInside = itop.labelScore(split, end, c, right)
                      val spanScore = validSpan.scoreBinaryRule(begin, split, end, r)
                      val bOutside = score + cInside + spanScore
                      val cOutside = score + bInside + spanScore
                      if(!java.lang.Double.isInfinite(bOutside) && !java.lang.Double.isInfinite(cOutside)) {
                        outside.top.enter(begin, split, b, h, bOutside)
                        outside.top.enter(split, end, c, right, cOutside)
                      }

                      split += 1
                    }
                  }
                  right += 1
                } // end extend right/left child is head
              }

              if(grammar.isRightRule(r)) {
                var left = begin
                while(left < h) {
                  val narrowR = narrowRight(itop.encodeLabelPair(b, left))
                  val narrowL = narrowLeft(itop.encodeLabelPair(c, h))

                  val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                    0L
                  } else {
                    val trueX = wideLeft(itop.encodeLabelPair(c, h))
                    val trueMin = if(narrowR > trueX) narrowR else trueX
                    val wr = wideRight(itop.encodeLabelPair(b, left))
                    val trueMax = if(wr < narrowL) wr else narrowL
                    if(trueMin > narrowL || trueMin > trueMax)  0L
                    else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
                  }
                  var split = (feasibleSpan >> 32).toInt
                  val endSplit = feasibleSpan.toInt // lower 32 bits
                  if (split < endSplit) {
                  val ruleScore = spec.scoreLeftComplement(r, h, left)
                    while(split < endSplit) {
                      val score = aScore + ruleScore
                      val bInside = itop.labelScore(begin, split, b, left)
                      val cInside = itop.labelScore(split, end, c, h)
                      val spanScore = validSpan.scoreBinaryRule(begin, split, end, r)
                      val bOutside = score + cInside + spanScore
                      val cOutside = score + bInside + spanScore
                      if(!java.lang.Double.isInfinite(bOutside) && !java.lang.Double.isInfinite(cOutside)) {
                        outside.top.enter(begin, split, b, left, bOutside)
                        outside.top.enter(split, end, c, h, cOutside)
                      }

                      split += 1
                    }
                  }
                  left += 1
                }
              }

            }
          }
        }
    }
    outside
  }


  private def updateInsideUnaries(chart: Chart[L], spec: grammar.Specialization, begin: Int, end: Int, validSpan: SpanScorer[L]) = {
    for(bh <- chart.bot.enteredLabelIndexes(begin, end)) {
      val b = chart.bot.decodeLabelPart(bh)
      val h = chart.bot.decodeHeadPart(bh)
      val bScore = chart.bot.labelScore(begin, end, bh)
      val rules = grammar.indexedUnaryRulesWithChild(b)
      var j = 0
      while(j < rules.length) {
        val a = grammar.indexedRules(rules(j)).parent
        val aScore: Double = spec.scoreUnary(rules(j), h)
        val prob: Double = aScore + bScore + validSpan.scoreUnaryRule(begin, end, rules(j))
        if(prob != Double.NegativeInfinity) {
          chart.top.enter(begin, end, a, h, prob)
        }
        j += 1
      }
    }

  }

  private def updateOutsideUnaries(outside: Chart[L], inside: ParseChart[L], spec: grammar.Specialization, begin: Int, end: Int, validSpan: SpanScorer[L]) = {
    for(ah <- outside.top.enteredLabelIndexes(begin, end)) {
      val a = outside.top.decodeLabelPart(ah)
      val h = outside.top.decodeHeadPart(ah)
      val aScore = outside.top.labelScore(begin, end, ah)

      var j = 0
      val rules = grammar.indexedUnaryRulesWithParent(a)
      while(j < rules.length) {
        val b = grammar.child(rules(j))
        val bScore = spec.scoreUnary(rules(j), h)
        val prob = aScore + bScore + validSpan.scoreUnaryRule(begin, end, rules(j))
        if(prob != Double.NegativeInfinity) {
          outside.bot.enter(begin, end, b, h, prob)
        }
        j += 1
      }
    }

  }
}