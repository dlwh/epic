package scalanlp.parser

import scalanlp.util.Index
import scalanlp.parser.ParseChart.Factory


case class MultiscaleHierarchy[L](index: Index[L],
                                  finalRefinements: Array[Array[Int]],
                                  oneStepRefinements: Array[Array[Int]],
                                  postorderTraversal: Array[Int]) {
  val maxOneStep = oneStepRefinements.iterator.map(_.length).max
}


/**
 * 
 * @author dlwh
 */
class MultiscaleChartBuilder[+Chart[X]<:ParseChart[X],
                             L,
                             W](val root: L,
                                val lexicon: Lexicon[L,W],
                                val grammar: Grammar[L],
                                val hierarchy: MultiscaleHierarchy[L],
                                chartFactory: Factory[Chart] = ParseChart.viterbi) extends ChartBuilder[Chart,L,W] {
   def withCharts[Chart[X]<:ParseChart[X]](factory: ParseChart.Factory[Chart]):ChartBuilder[Chart,L,W] = {
    new CKYChartBuilder[Chart,L,W](root,lexicon,grammar,factory)
  }

  def updateHierarchy(scores: ParseChart[L]#ChartScores, begin: Int, end: Int) {
    import hierarchy._
    val array = new Array[Double](maxOneStep)
    for(coarser <- postorderTraversal) {
      var i = 0
      val ref = oneStepRefinements(coarser)
      while(i < ref.length) {
        array(i) = scores.labelScore(begin,end,ref(i))
        i += 1
      }
      scores.enter(begin,end,coarser,array,ref.length)
    }

  }

  def buildInsideChart(s: Seq[W],
                       validSpan: SpanScorer[L] = SpanScorer.identity):Chart[L] = {
    val chart = chartFactory(grammar.labelEncoder,s.length)

    for{i <- 0 until s.length} {
      var foundSomething = false
      for {
        (a,wScore) <- lexicon.tagScores(s(i)).nonzero.pairs.iterator if !wScore.isInfinite && !wScore.isNaN
        ai = grammar.labelIndex(a)
        (spanScore:Double) = validSpan.scoreSpan(i,i+1,ai)
        if spanScore != Double.NegativeInfinity
      } {
        var j = 0;
        val ref = hierarchy.finalRefinements(ai)
        while(j < ref.length) {
          chart.bot.enter(i,i+1,ref(j),wScore + spanScore)
          j += 1
        }
        foundSomething = true
      }
      if(!foundSomething) {
        val spanScores = lexicon.tagScores(s(i)).nonzero.pairs.iterator.map { case (k,v) => (k,validSpan.scoreSpan(i,i+1,grammar.labelIndex(k)))}.toIndexedSeq
        sys.error("Couldn't score " + s(i) + " " + lexicon.tagScores(s(i)) + "spans: " + spanScores)
      }

      updateHierarchy(chart.bot,i,i+1)
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
        if(offset > 0) {
          var i = 0
          val ref = hierarchy.finalRefinements(a)
          val sum = chart.sum(scoreArray,offset)
          while(i < ref.length) {
            chart.bot.enter(begin,end,ref(i),sum)
            i += 1
          }
        }

      }
      updateHierarchy(chart.bot,begin,end)
      updateInsideUnaries(chart,begin,end,validSpan)
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
      if(span > 1) {
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
                  { // update b
                    var i = 0
                    val ref = hierarchy.finalRefinements(b)
                    while(i < ref.length) {
                      outside.top.enter(begin,end,ref(i),bOutside)
                      i += 1
                    }
                  }
                  {
                    var i = 0
                    val ref = hierarchy.finalRefinements(c)
                    while(i < ref.length) {
                      outside.top.enter(begin,end,ref(i),cOutside)
                      i += 1
                    }
                  }
                }

                split += 1
              }
            }
          }
        }
        updateHierarchy(outside.top,begin,end)
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
        val a = grammar.parent(rules(j))
        val aScore: Double = grammar.ruleScore(rules(j))
        val prob: Double = aScore + bScore + validSpan.scoreUnaryRule(begin, end, rules(j))
        if(prob != Double.NegativeInfinity) {
          var i = 0
          val refinements = hierarchy.finalRefinements(a)
          while(i < refinements.length) {
            val fa = refinements(i)
            chart.top.enter(begin, end, fa, prob)
            i += 1
          }
        }
        j += 1
      }
    }
    updateHierarchy(chart.top,begin,end)
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
          var i = 0
          val refinements = hierarchy.finalRefinements(b)
          while(i < refinements.length) {
            val fb = refinements(i)
            outside.bot.enter(begin,end,fb, prob)
            i += 1
          }
        }
        j += 1
      }
    }

    updateHierarchy(outside.bot,begin,end)
  }

}