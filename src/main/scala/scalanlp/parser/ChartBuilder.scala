package scalanlp.parser

import ParseChart.Factory

/**
 * Trait for things that parse. So far, we only have CKYChartBuilder
 */
trait ChartBuilder[+Chart[X]<:ParseChart[X],L,W] {
  /**
   * Given a sentence s, fills a parse chart with inside scores.
   * validSpan can be activeSize as a filter to insist that certain ones are valid.
   */
  def buildInsideChart(s: Seq[W], validSpan: SpanScorer[L] = SpanScorer.identity):Chart[L]
  /**
   * Given an inside chart, fills the passed-in outside parse chart with inside scores.
   */
  def buildOutsideChart(inside: ParseChart[L], validSpan: SpanScorer[L] = SpanScorer.identity):Chart[L]

  def grammar: Grammar[L]
  def root: L
  def lexicon:Lexicon[L,W]

  def index = grammar.labelIndex

  /**
   * Change the kind of parse chart that gets built
   */
  def withCharts[Chart[X]<:ParseChart[X]](factory: ParseChart.Factory[Chart]):ChartBuilder[Chart,L,W]
}

object ChartBuilder {
  def apply[Chart[X]<:ParseChart[X],L,W](root: L, lexicon: Lexicon[L,W],
                                         grammar: Grammar[L],
                                         chartFactory: Factory[Chart] = ParseChart.viterbi) = {
    new CKYChartBuilder[Chart,L,W](root,lexicon,grammar,chartFactory)
  }

}

/**
 * Implements the CKY algorithm for parsing a weighted pcfg.
 */
@SerialVersionUID(1)
class CKYChartBuilder[Chart[X]<:ParseChart[X], L,W](val root: L,
                                              val lexicon: Lexicon[L,W],
                                              val grammar: Grammar[L],
                                              chartFactory: Factory[Chart] = ParseChart.viterbi)
        extends ChartBuilder[Chart,L,W] with Serializable {

  def withCharts[Chart[X]<:ParseChart[X]](factory: ParseChart.Factory[Chart]):ChartBuilder[Chart,L,W] = {
    new CKYChartBuilder[Chart,L,W](root,lexicon,grammar,factory)
  }

  def buildInsideChart(s: Seq[W],
                       validSpan: SpanScorer[L] = SpanScorer.identity):Chart[L] = {
    val chart = chartFactory(grammar.labelEncoder,s.length)

    for{i <- 0 until s.length} {
      var foundSomething = false
      for {
        (a,wScore) <- lexicon.tagScores(s(i)).nonzero.pairs.iterator if !wScore.isInfinite && !wScore.isNaN
        ai = grammar.labelIndex(a)
        (spanScore:Double) = 0.0 //validSpan.scoreSpan(i,i+1,ai)
        if spanScore != Double.NegativeInfinity
      } {
        chart.bot.enter(i,i+1,ai,wScore + spanScore)
        foundSomething = true
      }
      if(!foundSomething) {
        val spanScores = lexicon.tagScores(s(i)).nonzero.pairs.iterator.map { case (k,v) => (k,validSpan.scoreSpan(i,i+1,grammar.labelIndex(k)))}.toIndexedSeq
        sys.error("Couldn't score " + s(i) + " " + lexicon.tagScores(s(i)) + "spans: " + spanScores)
      }

      updateInsideUnaries(chart,i,i+1, validSpan)
    }

    val maxSpanLength = s.length min 100
    // 100 = max span length
    val scoreArray = new Array[Double](grammar.maxNumBinaryRulesForParent * maxSpanLength * s.length)

    val top = chart.top

    // a -> bc over [begin,split,end)
    for {
      span <- 2 to (maxSpanLength)
      begin <- 0 to (s.length - span)
      end = begin + span
    } {
      for ( a <- 0 until grammar.labelIndex.size) {
        val passScore = validSpan.scoreSpan(begin,end,a)
        var offset = 0 // into scoreArray
        if(!passScore.isInfinite) {
          var ruleIndex = 0 // into rules
          val rules = grammar.indexedBinaryRulesWithParent(a)
          while(ruleIndex < rules.length) {
            val r = rules(ruleIndex)
            val b = grammar.leftChild(r)
            val c = grammar.rightChild(r)
            val ruleScore = grammar.ruleScore(r)
            ruleIndex += 1
            val feasibleSpan = top.feasibleSpanX(begin, end, b, c)
            var split = (feasibleSpan >> 32).toInt
            val endSplit = feasibleSpan.toInt // lower 32 bits
            while(split < endSplit) {
              val bScore = chart.top.labelScore(begin, split,b)
              val cScore = chart.top.labelScore(split, end, c)
              val spanScore = validSpan.scoreBinaryRule(begin,split,end,r) + passScore
              val totalA = ruleScore + spanScore
              val prob = bScore + cScore + totalA
              if(!java.lang.Double.isInfinite(prob)) {
                scoreArray(offset) = prob
                offset += 1
              }
              split += 1
            }
          }
        }
        // done updating vector, do an enter:
        if(offset > 0)
          chart.bot.enter(begin,end,a,scoreArray,offset)

      }
      updateInsideUnaries(chart,begin,end, validSpan)
    }
    chart
  }


  def buildOutsideChart(inside: ParseChart[L],
                         validSpan: SpanScorer[L] = SpanScorer.identity):Chart[L] = {
    val length = inside.length
    val outside = chartFactory(grammar.labelEncoder,length)
    outside.top.enter(0,inside.length,grammar.labelIndex(root),0.0)
    val itop = inside.top
    val ibot = inside.top
    for {
      span <- length until 0 by (-1)
      begin <- 0 to (length-span)
    } {
      val end = begin + span
      updateOutsideUnaries(outside,inside, begin,end, validSpan)
      if(span > 1)
        // a ->  bc  [begin,split,end)
        for ( a <- outside.bot.enteredLabelIndexes(begin,end) ) {
          val aScore = outside.bot.labelScore(begin,end,a) + validSpan.scoreSpan(begin,end,a)
          if (!aScore.isInfinite) {
            val rules = grammar.indexedBinaryRulesWithParent(a)
            var br = 0;
            while(br < rules.length) {
              val r = rules(br)
              val b = grammar.leftChild(r)
              val c = grammar.rightChild(r)
              val ruleScore = grammar.ruleScore(r)
              br += 1
              val feasibleSpan = itop.feasibleSpanX(begin, end, b, c)
              var split = (feasibleSpan >> 32).toInt
              val endSplit = feasibleSpan.toInt // lower 32 bits
              while(split < endSplit) {
                val score = aScore + ruleScore
                val bInside = itop.labelScore(begin,split,b)
                val cInside = itop.labelScore(split,end,c)
                val spanScore = validSpan.scoreBinaryRule(begin,split,end,r)
                val bOutside = score + cInside + spanScore
                val cOutside = score + bInside + spanScore
                if(!java.lang.Double.isInfinite(bOutside) && !java.lang.Double.isInfinite(cOutside)) {
                  outside.top.enter(begin,split,b,bOutside)
                  outside.top.enter(split,end,c,cOutside)
                }

                split += 1
              }
            }
          }
        }
    }
    outside
  }

  private def updateInsideUnaries(chart: ParseChart[L], begin: Int, end: Int, validSpan: SpanScorer[L]) = {
    for(b <- chart.bot.enteredLabelIndexes(begin,end)) {
      val bScore = chart.bot.labelScore(begin,end,b)
      val rules = grammar.indexedUnaryRulesWithChild(b)
      var j = 0
      while(j < rules.length) {
        val a = grammar.indexedRules(rules(j)).parent
        val aScore: Double = grammar.ruleScore(rules(j))
        val prob: Double = aScore + bScore + validSpan.scoreUnaryRule(begin, end, rules(j))
        if(prob != Double.NegativeInfinity) {
          chart.top.enter(begin, end, a, prob)
        }
        j += 1
      }
    }

  }

  private def updateOutsideUnaries(outside: ParseChart[L], inside: ParseChart[L], begin: Int, end: Int, validSpan: SpanScorer[L]) = {
    for(a <- outside.top.enteredLabelIndexes(begin,end)) {
      val aScore = outside.top.labelScore(begin,end,a)

      var j = 0
      val rules = grammar.indexedUnaryRulesWithParent(a)
      while(j < rules.length) {
        val b = grammar.child(rules(j))
        val bScore = grammar.ruleScore(rules(j))
        val prob = aScore + bScore + validSpan.scoreUnaryRule(begin,end,rules(j))
        if(prob != Double.NegativeInfinity && inside.bot.labelScore(begin,end,b) != Double.NegativeInfinity) {
          outside.bot.enter(begin,end,b, prob)
        }
        j += 1
      }
    }

  }
}

object CKYChartBuilder {
  def apply[L,W](root: L, lexicon: Lexicon[L,W], grammar: Grammar[L]) = {
    new CKYChartBuilder[ParseChart.ViterbiParseChart,L,W](root,lexicon,grammar,ParseChart.viterbi)
  }
}
