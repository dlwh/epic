package scalanlp.parser
import scalanlp.parser.ParseChart.Factory
import scalanlp.util.TypeTags

/**
 * Builds charts over a sentence, giving chart marginals
 * @param grammar weighted grammar used for computing scores
 * @param chartFactory factory to build th right kind of chart
 * @tparam Chart the kind of chart to build
 * @tparam L label type
 * @tparam W word type
 */
class CKYChartBuilder[+Chart[X]<:ParseChart[X], L, W](val grammar: WeightedGrammar[L, W],
                                                      val chartFactory: Factory[Chart]) extends ChartBuilder[Chart, L, W] {

  def charts(words: Seq[W]) = {
    val spec = grammar.specialize(words)
    val inside = buildInsideChart(spec)
    val outside = buildOutsideChart(inside, spec)
    val partition = rootScore(spec, inside)

    new ChartMarginal[Chart, L, W](grammar.grammar, spec, inside, outside, partition)
  }


  def withCharts[Chart2[X] <: ParseChart[X]](prob: Factory[Chart2]) = new CKYChartBuilder(grammar, prob)

  private def rootScore(spec: grammar.Specialization, inside: ParseChart[L]) = {
    val rootScores = new Array[Double](spec.validLabelRefinements(0, inside.length, rootIndex).length)
    var offset = 0
    for(ref <- inside.top.enteredLabelRefinements(0, inside.length, rootIndex)) {
      val score = inside.top.labelScore(0, inside.length, rootIndex, ref)
      if(score != Double.NegativeInfinity) {
        rootScores(offset) = score
        offset += 1
      }
    }
    inside.sum(rootScores, offset)
  }

  private def buildInsideChart(spec: grammar.Specialization): Chart[L] = {
    import spec.words
    val chart = chartFactory(grammar.labelIndex, Array.tabulate(grammar.labelIndex.size)(spec.numValidRefinements), words.length)

    for{i <- 0 until words.length} {
      var foundSomething = false
      for {
        a <- spec.validTagsFor(i)
        ref <- spec.validLabelRefinements(i, i+ 1, a)
      } {
        val score:Double = spec.scoreSpan(i, i+1, a, ref)
        if (score != Double.NegativeInfinity) {
          chart.bot.enter(i, i+1, a, ref, score)
          foundSomething = true
        }
      }

      updateInsideUnaries(chart, spec,  i, i+1)
    }

    // buffer. Set to 1000. If we ever fill it up, accumulate everything into
    // elem 0 and try again
    val scoreArray = new Array[Double](1000)

    val top = chart.top

    // a -> bc over [begin, split, end)
    for {
      span <- 2 to words.length
      begin <- 0 to (words.length - span)
      end = begin + span
    } {
      val narrowRight = top.narrowRight(begin)
      val narrowLeft = top.narrowLeft(end)
      val wideRight = top.wideRight(begin)
      val wideLeft = top.wideLeft(end)
      for ( ai <- 0 until grammar.labelIndex.size; refA <- spec.validLabelRefinements(begin, end, TypeTags.tag(ai))) {
        val a = ai
        val passScore = spec.scoreSpan(begin, end, a, refA)
        var offset = 0 // into scoreArray
        if(!passScore.isInfinite) {
          var ruleIndex = 0 // into rules
          val rules = grammar.grammar.indexedBinaryRulesWithParent(a)
          while(ruleIndex < rules.length) {
            val r = rules(ruleIndex)
            val b = grammar.grammar.leftChild(r)
            val c = grammar.grammar.rightChild(r)
            ruleIndex += 1
            for( refR <- spec.validRuleRefinementsGivenParent(begin, end, r, refA)) {
              val refB = spec.leftChildRefinement(r, refR)
              val refC = spec.rightChildRefinement(r, refR)
              // narrowR etc is hard to understand, and should be a different methood
              // but caching the arrays speeds things up by 20% or more...
              // so it's inlined.
              //
              // See [[ParseChart]] for what these labels mean
              val narrowR:Int = narrowRight(b)(refB)
              val narrowL:Int = narrowLeft(c)(refC)

              val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                0L
              } else {
                val trueX:Int = wideLeft(c)(refC)
                val trueMin = if(narrowR > trueX) narrowR else trueX
                val wr:Int = wideRight(b)(refB)
                val trueMax = if(wr < narrowL) wr else narrowL
                if(trueMin > narrowL || trueMin > trueMax)  0L
                else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
              }
              var split = (feasibleSpan >> 32).toInt
              val endSplit = feasibleSpan.toInt // lower 32 bits
              while(split < endSplit) {
                val ruleScore = spec.scoreBinaryRule(begin, split, end, r, refR)
                val bScore = chart.top.labelScore(begin, split, b, refB)
                val cScore = chart.top.labelScore(split, end, c, refC)
                val totalA = ruleScore + passScore
                val prob = bScore + cScore + totalA
                if(!java.lang.Double.isInfinite(prob)) {
                  scoreArray(offset) = prob
                  offset += 1
                  // buffer full
                  if(offset == scoreArray.length) {
                    scoreArray(0) = chart.sum(scoreArray, offset)
                    offset = 1
                  }
                }
                split += 1
              }
            }
          }
        }
        // done updating vector, do an enter:
        if(offset > 0) {
          chart.bot.enterSum(begin, end, a, refA, scoreArray, offset)
        }

      }
      updateInsideUnaries(chart, spec, begin, end)
    }
    chart
  }


  private def buildOutsideChart(inside: ParseChart[L], spec: grammar.Specialization):Chart[L] = {
    val length = inside.length
    val outside = chartFactory(grammar.labelIndex, Array.tabulate(grammar.labelIndex.size)(spec.numValidRefinements), length)
    for(refRoot <- spec.validLabelRefinements(0, inside.length, rootIndex)) {
      outside.top.enter(0, inside.length, rootIndex, refRoot, 0.0)
    }
    val itop = inside.top
    for {
      span <- length until 0 by (-1)
      begin <- 0 to (length-span)
    } {
      val end = begin + span
      val narrowRight = itop.narrowRight(begin)
      val narrowLeft = itop.narrowLeft(end)
      val wideRight = itop.wideRight(begin)
      val wideLeft = itop.wideLeft(end)
      updateOutsideUnaries(outside, inside, spec, begin, end)
      if(span > 1)
      // a ->  bc  [begin, split, end)
        for ( a <- outside.bot.enteredLabelIndexes(begin, end); refA <- outside.bot.enteredLabelRefinements(begin, end, a) ) {
          val aScore:Double = outside.bot.labelScore(begin, end, a, refA) + spec.scoreSpan(begin, end, a, refA)
          if (!aScore.isInfinite) {
            val rules = grammar.grammar.indexedBinaryRulesWithParent(a)
            var br = 0;
            while(br < rules.length) {
              val r = rules(br)
              val b = grammar.grammar.leftChild(r)
              val c = grammar.grammar.rightChild(r)
              br += 1
              // this is too slow, so i'm having to inline it.
              //              val feasibleSpan = itop.feasibleSpanX(begin, end, b, c)
              for(refR <- spec.validRuleRefinementsGivenParent(begin, end, r, refA)) {
                val refB = spec.leftChildRefinement(r, refR)
                val refC = spec.rightChildRefinement(r, refR)
                val narrowR:Int = narrowRight(b)(refB)
                val narrowL:Int = narrowLeft(c)(refC)

                val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                  0L
                } else {
                  val trueX:Int = wideLeft(c)(refC)
                  val trueMin = if(narrowR > trueX) narrowR else trueX
                  val wr:Int = wideRight(b)(refB)
                  val trueMax = if(wr < narrowL) wr else narrowL
                  if(trueMin > narrowL || trueMin > trueMax)  0L
                  else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
                }
                var split = (feasibleSpan >> 32).toInt
                val endSplit = feasibleSpan.toInt // lower 32 bits
                while(split < endSplit) {
                  val ruleScore = spec.scoreBinaryRule(begin, split, end, r, refR)
                  val score = aScore + ruleScore
                  val bInside = itop.labelScore(begin, split, b, refB)
                  val cInside = itop.labelScore(split, end, c, refC)
                  val bOutside = score + cInside
                  val cOutside = score + bInside
                  if(!java.lang.Double.isInfinite(bOutside) && !java.lang.Double.isInfinite(cOutside)) {
                    outside.top.enter(begin, split, b, refB, bOutside)
                    outside.top.enter(split, end, c, refC, cOutside)
                  }

                  split += 1
                }
              }
            }
          }
        }
    }
    outside
  }


  private def updateInsideUnaries(chart: ParseChart[L], spec: grammar.Specialization, begin: Int, end: Int) = {
    for(bi <- chart.bot.enteredLabelIndexes(begin, end); refB <- chart.bot.enteredLabelRefinements(begin, end, bi)) {
      val b = bi
      val bScore = chart.bot.labelScore(begin, end, b, refB)
      val rules = grammar.grammar.indexedUnaryRulesWithChild(b)
      var j = 0
      while(j < rules.length) {
        val r = rules(j)
        val a = grammar.grammar.parent(r)
        for(refR <- spec.validUnaryRuleRefinementsGivenChild(begin, end, r, refB)) {
          val refA = spec.parentRefinement(r, refR)
          val ruleScore: Double = spec.scoreUnaryRule(begin, end, r, refR)
          val prob: Double = bScore + ruleScore
          if(prob != Double.NegativeInfinity) {
            chart.top.enter(begin, end, a, refA, prob)
          }
        }
        j += 1
      }
    }

  }

  private def updateOutsideUnaries(chart: ParseChart[L], inside: ParseChart[L], spec: grammar.Specialization, begin: Int, end: Int) = {
    for(ai <- chart.top.enteredLabelIndexes(begin, end); refA <- chart.top.enteredLabelRefinements(begin, end, ai)) {
      val a = ai
      val bScore = chart.top.labelScore(begin, end, a, refA)
      val rules = grammar.grammar.indexedUnaryRulesWithParent(a)
      var j = 0
      while(j < rules.length) {
        val r = rules(j)
        val b = grammar.grammar.child(r)
        for(refR <- spec.validRuleRefinementsGivenParent(begin, end, rules(j), refA)) {
          val refB = spec.childRefinement(rules(j), refR)
          val ruleScore: Double = spec.scoreUnaryRule(begin, end, rules(j), refR)
          val prob: Double = bScore + ruleScore
          if(prob != Double.NegativeInfinity) {
            chart.bot.enter(begin, end, b, refB, prob)
          }
        }
        j += 1
      }
    }

  }
}

object CKYChartBuilder {
  def apply[L, W, Chart[X]<:ParseChart[X]](grammar: WeightedGrammar[L,W], chartFactory: ParseChart.Factory[Chart]) = {
    new CKYChartBuilder(grammar, chartFactory)
  }
}
