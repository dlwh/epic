package scalanlp.parser

import scala.collection.mutable.BitSet
import scalala.tensor.counters.Counters.DoubleCounter
import scalanlp.trees.Tree

class ChartParser[L,W](val root: L, val lexicon: Lexicon[L,W], val grammar: Grammar[L]) extends Parser[L,W] {

  // Score is a vector of scores whose indices are nonterms or preterms
  private def updateInsideUnaries(chart: InsideParseChart[L], begin:Int, end: Int) = {
    var recheck = grammar.mkArray[Int];
    var check = grammar.mkArray[Int];

    var used = 0;
    for(idx <- chart.enteredLabelIndexes(begin,end)) {
      recheck(used) = idx;
      used += 1;
    }
    var old_used = used;

    val set = new BitSet();

    val max_iter = 5;
    var iter = 0;
    while(iter < max_iter) {
      val tmp = check;
      check = recheck;
      recheck = tmp;
      used = 0;
      set.clear();

      var i = 0;
      while(i < old_used) {
        val b = check(i);
        i += 1;
        val bScore = chart.labelScore(begin,end,b);

        var j = 0;
        val parentVector = grammar.unaryRulesByIndexedChild(b);
        while(j < parentVector.used) {
          val a = parentVector.index(j);
          val aScore = parentVector.data(j);
          j += 1
          val prob = aScore + bScore;
          if(prob > chart.labelScore(begin,end,a)) {
            chart.enterUnary(begin,end,a,b,prob);
            if(!set(a)) {
              set += a;
              recheck(used) = a;
              used += 1;
            }
          }
        }
      }

      old_used = used;

      iter += 1;
    }

  }

  private def updateOutsideUnaries(outside: OutsideParseChart[L], begin: Int, end: Int) = {
        var recheck = grammar.mkArray[Int];
    var check = grammar.mkArray[Int];

    var used = 0;
    for(idx <- outside.enteredLabelIndexes(begin,end)) {
      recheck(used) = idx;
      used += 1;
    }
    var old_used = used;

    val set = new BitSet();

    val max_iter = 5;
    var iter = 0;
    while(iter < max_iter) {
      val tmp = check;
      check = recheck;
      recheck = tmp;
      used = 0;
      set.clear();

      var i = 0;
      while(i < old_used) {
        val a = check(i);
        i += 1;
        val aScore = outside.labelScore(begin,end,a);

        var j = 0;
        val childVector = grammar.unaryRulesByIndexedParent(a);
        while(j < childVector.used) {
          val b = childVector.index(j);
          val bScore = childVector.data(j);
          j += 1
          val prob = aScore + bScore;
          if(prob > outside.labelScore(begin,end,b)) {
            outside.enter(begin,end,b,prob);
            if(!set(b)) {
              set += b;
              recheck(used) = b;
              used += 1;
            }
          }
        }
      }

      old_used = used;

      iter += 1
    }
  }

  def buildOutsideChart(inside: InsideParseChart[L]) = {
    val length = inside.length;
    val outside = new OutsideParseChart[L](grammar,inside.length);
    outside.enter(0,inside.length,grammar.index(root),0.0);
    for {
      begin <- 0 until length;
      end <- length until begin
    } {
      updateOutsideUnaries(outside,0,length);
      for {
        (b,binaryRules) <- grammar.allBinaryRules
        (c,parents) <- binaryRules
        (a,ruleScore) <- parents.activeElements
        if !ruleScore.isInfinite && !outside.labelScore(begin, end, a).isInfinite
        split <- (begin + 1) until (end-1)
        // TODO: figure out how to use feasible spans for outside
        //split <- inside.feasibleSpan(begin, end, b, c)
      } {
        val aOutside = outside.labelScore(begin, end, a) + ruleScore;
        val bInside = inside.labelScore(begin,split,b);
        val cInside = inside.labelScore(split,end,c);
        if(!bInside.isInfinite && !cInside.isInfinite) {
          val bOutside = aOutside + cInside;
          if(bOutside > outside.labelScore(begin,split,b)) {
            outside.enter(begin,split,b,bOutside);
          }

          val cOutside = aOutside + bInside;
          if(cOutside > outside.labelScore(split,end,b)) {
            outside.enter(split,end,c,cOutside);
          }
        }
      }
    }

    outside;
  }

  /** begin, end, label */
  type LabeledSpanFilter = (Int,Int,Int) => Boolean

  def buildInsideChart(s: Seq[W], filter: LabeledSpanFilter = ( (_,_,_) => true) ): InsideParseChart[L] = {
    val chart = new InsideParseChart(grammar,s.length);

    for{i <- 0 until s.length} {
      for ( (a,wScore) <- lexicon.tagScores(s(i))
            if !wScore.isInfinite) {
        assert(a != null);
        chart.enterTerm(i,i+1,a,wScore);
      }

      updateInsideUnaries(chart,i,i+1);
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
        if filter(begin,split,b) && filter(split,end,c)
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
              val prob = bScore + cScore + aScore;
              if(prob > chart.labelScore(begin,end,a)) {
                chart.enterBinary(begin,split,end,a,b,c,prob);
              }

            }
          }
        }
      }
      updateInsideUnaries(chart,begin,end);
    }
    chart;
  }

  def scores(s: Seq[W]) = {
    val chart = buildInsideChart(s);
    try {
      val bestParse = chart.buildTree(0,s.length,grammar.index(root));
      val c = DoubleCounter[Tree[L]]();
      c(bestParse) = chart.labelScore(0, s.length, root);
      c;
    } catch {
      case e =>
        chart.dumpChart();
        throw e;
    }
  }
}