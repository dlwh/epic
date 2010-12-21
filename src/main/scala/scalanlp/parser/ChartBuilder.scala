package scalanlp.parser

import ParseChart.Factory;

trait ChartBuilder[+Chart[X]<:ParseChart[X],L,W] {
  /**
   * Given a sentence s, fills a parse chart with inside scores.
   * validSpan can be used as a filter to insist that certain ones are valid.
   */
  def buildInsideChart(s: Seq[W], validSpan: SpanScorer = SpanScorer.identity):Chart[L];
  /**
   * Given an inside chart, fills the passed-in outside parse chart with inside scores.
   */
  def buildOutsideChart(inside: ParseChart[L], validSpan: SpanScorer = SpanScorer.identity):Chart[L];

  def grammar: Grammar[L];
  def root: L;
  def lexicon:Lexicon[L,W];

  def index = grammar.index;

  def withCharts[Chart[X]<:ParseChart[X]](factory: ParseChart.Factory[Chart]):ChartBuilder[Chart,L,W];
}

object ChartBuilder {
  def apply[Chart[X]<:ParseChart[X],L,W](root: L, lexicon: Lexicon[L,W],
                                         grammar: Grammar[L],
                                         chartFactory: Factory[Chart] = ParseChart.viterbi) = {
    new CKYChartBuilder[Chart,L,W](root,lexicon,grammar,chartFactory);
  }

}

@serializable
@SerialVersionUID(1)
class CKYChartBuilder[Chart[X]<:ParseChart[X], L,W](val root: L,
                                              val lexicon: Lexicon[L,W],
                                              val grammar: Grammar[L],
                                              chartFactory: Factory[Chart] = ParseChart.viterbi)
        extends ChartBuilder[Chart,L,W] {

  def withCharts[Chart[X]<:ParseChart[X]](factory: ParseChart.Factory[Chart]):ChartBuilder[Chart,L,W] = {
    new CKYChartBuilder[Chart,L,W](root,lexicon,grammar,factory);
  }

  def buildInsideChart(s: Seq[W],
                       validSpan: SpanScorer = SpanScorer.identity):Chart[L] = {
    val chart = chartFactory(grammar,s.length);

    for{i <- 0 until s.length} {
      var foundSomething = false;
      for {
        (a,wScore) <- lexicon.tagScores(s(i))
        if !wScore.isInfinite && !wScore.isNaN;
        ai = grammar.index(a);
        (spanScore:Double) = validSpan.scoreLexical(i,i+1,ai)
        if spanScore != Double.NegativeInfinity
      } {
        chart.bot.enter(i,i+1,ai,wScore + spanScore);
        foundSomething = true;
      }
      if(!foundSomething) {
        val spanScores = lexicon.tagScores(s(i)).iterator.map { case (k,v) => (k,validSpan.scoreLexical(i,i+1,grammar.index(k)))} toIndexedSeq;
        error("Couldn't score " + s(i) + " " + lexicon.tagScores(s(i)) + "spans: " + spanScores);
      }

      updateInsideUnaries(chart,i,i+1, validSpan);
    }

    for {
      span <- 2 to s.length;
      begin <- 0 to (s.length - span);
      end = begin + span
    } {
      for {
        (b,binaryRules) <- grammar.allBinaryRules;
        //if chart.top.canStartHere(begin, end, b);
        (c,parentVector) <- binaryRules;
        split <- chart.top.feasibleSpan(begin, end, b, c)
      } {
        val bScore = chart.top.labelScore(begin, split,b);
        if (!bScore.isInfinite) {
          val cScore = chart.top.labelScore(split, end, c)
          if (!cScore.isInfinite) {
            var i = 0;
            while(i < parentVector.used) {
              val a = parentVector.index(i);
              val spanScore = validSpan.scoreBinaryRule(begin,split,end,a,b,c);
              if(spanScore != Double.NegativeInfinity) {
                val aScore = parentVector.data(i) + spanScore;
                if(aScore != Double.NegativeInfinity) {
                  val prob = bScore + cScore + aScore;
                  chart.bot.enter(begin,end,a,prob);
                }
              }
              i += 1;
            }
          }
        }
      }
      updateInsideUnaries(chart,begin,end, validSpan);
    }
    chart;
  }


  def buildOutsideChart(inside: ParseChart[L],
                         validSpan: SpanScorer = SpanScorer.identity):Chart[L] = {
    val length = inside.length;
    val outside = chartFactory(grammar,length);
    outside.top.enter(0,inside.length,grammar.index(root),0.0);
    for {
      span <- length until 0 by (-1)
      begin <- 0 to (length-span)
    } {
      val end = begin + span;
      updateOutsideUnaries(outside,begin,end, validSpan);
      for {
        a <- outside.bot.enteredLabelIndexes(begin,end);
        if !inside.bot.labelScore(begin,end,a).isInfinite
        (b,binaryRules) <- grammar.binaryRulesByIndexedParent(a);
        //if inside.top.canStartHere(begin,end,b)
        (c,ruleScore) <- binaryRules.activeElements
        split <- inside.top.feasibleSpan(begin, end, b, c)
      } {
        val score = outside.bot.labelScore(begin,end,a) + ruleScore
        val bInside = inside.top.labelScore(begin,split,b);
        if(!java.lang.Double.isInfinite(bInside)) {
          val cInside = inside.top.labelScore(split,end,c);
          if(!java.lang.Double.isInfinite(cInside)) {
            val spanScore = validSpan.scoreBinaryRule(begin,split,end,a,b,c);
            if(spanScore != Double.NegativeInfinity) {
              val bOutside = score + cInside + spanScore;
              outside.top.enter(begin,split,b,bOutside);

              val cOutside = score + bInside + spanScore;
              outside.top.enter(split,end,c,cOutside);
            }
          }
        }
      }
    }

    outside
  }

  private def updateInsideUnaries(chart: ParseChart[L], begin: Int, end: Int, validSpan: SpanScorer) = {
    for(b <- chart.bot.enteredLabelIndexes(begin,end)) {
      val bScore = chart.bot.labelScore(begin,end,b);
      val parentVector = grammar.unaryRulesByIndexedChild(b);
      var j = 0;
      while(j < parentVector.used) {
        val a = parentVector.index(j);
        val aScore = parentVector.data(j);
        val prob = aScore + bScore + validSpan.scoreUnaryRule(begin, end, a, b);
        if(prob != Double.NegativeInfinity) {
          chart.top.enter(begin,end,a, prob);
        }
        j += 1
      }
    }

  }

  private def updateOutsideUnaries(outside: ParseChart[L], begin: Int, end: Int, validSpan: SpanScorer) = {
    for(a <- outside.top.enteredLabelIndexes(begin,end)) {
      val aScore = outside.top.labelScore(begin,end,a);

      var j = 0;
      val childVector = grammar.unaryRulesByIndexedParent(a);
      while(j < childVector.used) {
        val b = childVector.index(j);
        val bScore = childVector.data(j);
        val prob = aScore + bScore + validSpan.scoreUnaryRule(begin,end,a,b);
        if(prob != Double.NegativeInfinity) {
          outside.bot.enter(begin,end,b, prob);
        }
        j += 1
      }
    }

  }
}

object CKYChartBuilder {
  def apply[L,W](root: L, lexicon: Lexicon[L,W], grammar: Grammar[L]) = {
    new CKYChartBuilder[ParseChart.ViterbiParseChart,L,W](root,lexicon,grammar,ParseChart.viterbi);
  }
}
