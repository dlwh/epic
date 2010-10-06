package scalanlp.parser

import scala.collection.mutable.BitSet
import scalala.tensor.counters.Counters.DoubleCounter
import scalanlp.trees.Tree

import ChartParser._;
import ParseChart.Factory;

trait ChartParser[Chart[X]<:ParseChart[X],L,W] extends Parser[L,W] {
  /**
   * Given a sentence s, fills a parse chart with inside scores.
   * validSpan can be used as a filter to insist that certain ones are valid.
   */
  def buildInsideChart(s: Seq[W], validSpan: SpanFilter = defaultFilter):Chart[L];
  /**
   * Given an inside chart, fills the passed-in outside parse chart with inside scores.
   */
  def buildOutsideChart(inside: ParseChart[L], validSpan: SpanFilter = defaultFilter):Chart[L];

  def grammar: Grammar[L];
  def root: L;
  def lexicon:Lexicon[L,W];


  def scores(s: Seq[W]) = {
    try {
      val chart = buildInsideChart(s);
      val bestParse = chart.buildTree(0,s.length,grammar.index(root));
      val c = DoubleCounter[Tree[L]]();
      c(bestParse) = chart.labelScore(0, s.length, root);
      c;
    } catch {
      case e =>
        throw e;
    }
  }
}

object ChartParser {
  /**
   *  A spanfilter decides whether or not a label is acceptible for a
   * given span. The parameters are begin, end, label, where label
   * is the index of the label in the grammar the chart parser is using.
   */
  type SpanFilter = (Int,Int,Int)=>Boolean;
  def defaultFilter(begin: Int, end: Int, label: Int) = true;
  // optimization: use the single version of defaultFilter by default so that we can
  // do a reference equality check and avoid the method call in the common case.
  // Yes, this showed up in a profile.
  val defaultFilterBoxed:SpanFilter = defaultFilter _;

  def apply[Chart[X]<:ParseChart[X],L,W](root: L, lexicon: Lexicon[L,W],
                                         grammar: Grammar[L],
                                         chartFactory: Factory[Chart] = ParseChart.viterbi) = {
    new CKYParser[Chart,L,W](root,lexicon,grammar,chartFactory);
  }

}

class CKYParser[Chart[X]<:ParseChart[X], L,W](val root: L,
                                              val lexicon: Lexicon[L,W],
                                              val grammar: Grammar[L],
                                              chartFactory: Factory[Chart] = ParseChart.viterbi)
        extends ChartParser[Chart,L,W] {

  private lazy val unaryClosure: UnaryRuleClosure = chartFactory.computeUnaryClosure(grammar);

  def buildInsideChart(s: Seq[W],
                       validSpan: SpanFilter = defaultFilterBoxed):Chart[L] = {
    val chart = chartFactory(grammar,s.length);

    for{i <- 0 until s.length} {
      for ( (a,wScore) <- lexicon.tagScores(s(i))
            if !wScore.isInfinite && !wScore.isNaN) {
        chart.enter(i,i+1,grammar.index(a),wScore);
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
        if chart.canStartHere(begin, end, b);
        (c,parentVector) <- binaryRules;
        split <- chart.feasibleSpan(begin, end, b, c)
      } {
        val bScore = chart.labelScore(begin, split,b);
        if (!bScore.isInfinite) {
          val cScore = chart.labelScore(split, end, c)
          if (!cScore.isInfinite) {
            var i = 0;
            while(i < parentVector.used) {
              val a = parentVector.index(i);
              val aScore = parentVector.data(i);
              i += 1;
              if((validSpan eq defaultFilterBoxed) || validSpan(begin,end,a)) {
                val prob = bScore + cScore + aScore;
                chart.enter(begin,end,a,prob);
              }
            }
          }
        }
      }
      updateInsideUnaries(chart,begin,end, validSpan);
    }
    chart;
  }


  def buildOutsideChart(inside: ParseChart[L],
                         validSpan: SpanFilter = defaultFilterBoxed):Chart[L] = {
    val length = inside.length;
    val outside = chartFactory(grammar,length);
    outside.enter(0,inside.length,grammar.index(root),0.0);
    for {
      span <- length until 0 by (-1)
      begin <- 0 to (length-span)
    } {
      val end = begin + span;
      updateOutsideUnaries(outside,begin,end, validSpan);
      for {
        a <- outside.enteredLabelIndexes(begin,end);
        if !inside.labelScore(begin,end,a).isInfinite
        (b,binaryRules) <- grammar.binaryRulesByIndexedParent(a);
        if inside.canStartHere(begin,end,b)
        (c,ruleScore) <- binaryRules.activeElements
        split <- (begin + 1) until end//inside.feasibleSpan(begin, end, b, c)
      } {
        val aOutside = outside.labelScore(begin, end, a) + ruleScore;
        val bInside = inside.labelScore(begin,split,b);
        val cInside = inside.labelScore(split,end,c);
        if(!bInside.isInfinite && !cInside.isInfinite) {
          val bOutside = aOutside + cInside;
          if((validSpan eq defaultFilterBoxed) || validSpan(begin,split,b))
            outside.enter(begin,split,b,bOutside);

          val cOutside = aOutside + bInside;
          if( (validSpan eq defaultFilterBoxed) || validSpan(split,end,c))
            outside.enter(split,end,c,cOutside);
        }
      }
    }

    outside;
  }

  private def updateInsideUnaries(chart: ParseChart[L], begin: Int, end: Int, validSpan: SpanFilter) = {
    val newMass = grammar.mkVector(Double.NegativeInfinity);
    for(b <- chart.enteredLabelIndexes(begin,end)) {
      val bScore = chart.labelScore(begin,end,b);
      val parentVector = unaryClosure.closeFromChild(b);
      var j = 0;
      while(j < parentVector.used) {
        val a = parentVector.index(j);
        if(a != b) {
          val aScore = parentVector.data(j);
          val prob = aScore + bScore;
          if(validSpan(begin,end,a)) {
            newMass(a) = chart.sum(newMass(a),prob);
          }
        }
        j += 1
      }
    }

    for((a,v) <- newMass.activeElements) {
      chart.enter(begin,end,a, v);
    }

  }

  private def updateOutsideUnaries(outside: ParseChart[L], begin: Int, end: Int, validSpan: SpanFilter) = {
    val newMass = grammar.mkVector(Double.NegativeInfinity);
    for(a <- outside.enteredLabelIndexes(begin,end)) {
      val aScore = outside.labelScore(begin,end,a);
      var j = 0;
      val childVector = unaryClosure.closeFromParent(a);
      while(j < childVector.used) {
        val b = childVector.index(j);
        if(a != b) {
          val bScore = childVector.data(j);
          val prob = aScore + bScore;
          if(validSpan(begin,end,b)) {
            newMass(b) = outside.sum(newMass(b),prob);
          }
        }
        j += 1
      }
    }

    for((a,v) <- newMass.activeElements) {
      outside.enter(begin,end,a, v);
    }
  }
}

object CKYParser {
  def apply[L,W](root: L, lexicon: Lexicon[L,W], grammar: Grammar[L]) = {
    new CKYParser[ParseChart.ViterbiParseChart,L,W](root,lexicon,grammar,ParseChart.viterbi);
  }
}