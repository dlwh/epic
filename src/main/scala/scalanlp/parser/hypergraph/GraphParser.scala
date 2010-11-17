package scalanlp.parser.hypergraph
/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/



import scala.collection.mutable.PriorityQueue
import scalala.tensor.counters.Counters.DoubleCounter
import scalanlp.trees.Span
import scalanlp.parser._;
import scalanlp.trees.Tree
import scala.util.control.Breaks._;
import scala.util.control.Breaks;


class GraphParser[L,W](root: L, lexicon: Lexicon[L,W], grammar: Grammar[L]) extends Parser[L,W] {

  sealed trait Item {
    def score: Double
    def span: Span
    def parent: Int
  }

  case class UnaryItem(parent: Int, child: Int, span: Span, score: Double) extends Item;
  case class BinaryItem(parent: Int, leftChild: Int, rightChild: Int, span: Span, split: Int, score: Double) extends Item;
  case class LexicalItem(l: Int, loc: Int, score: Double) extends Item {
    def parent = l;
    def span = new Span(loc,loc+1);
  }

  implicit val ordering: Ordering[Item] = Ordering.Double.on(_.score);

  val continues = new Breaks;

  def scores(s: Seq[W]) = {
    // make the chart:
    val chart = ParseChart.viterbi(grammar,s.length);
    val agenda = new PriorityQueue[Item];

    for( (w,i) <- s zipWithIndex;
        a <- lexicon.tags;
        wScore = lexicon.wordScore(a,s(i))
        if !wScore.isInfinite) {
      agenda += LexicalItem(grammar.index(a),i,wScore);
    }
    breakable {
      while(!agenda.isEmpty) {
        val top = agenda.dequeue;
        // enter it into the chart:
        var alreadyAdded = false;
        top match {
          case LexicalItem(parent, i, w) =>
            val span = top.span;
            if(chart.labelScore(span.start, span.end, parent).isInfinite) {
              chart.enterTerm(span.start, span.end, parent, w);
            } else {
              alreadyAdded = true; // already expanded a better node here
            }
          case UnaryItem(parent, child, span, w) =>
            if(chart.labelScore(span.start, span.end, parent).isInfinite) {
              chart.enterUnary(span.start, span.end, parent, child, w)
            } else {
              alreadyAdded = true; // already expanded a better node here
            }
          case BinaryItem(parent, lchild, rchild, span, split, w) =>
            if(chart.labelScore(span.start, span.end, parent).isInfinite) {
              chart.enterBinary(span.start, split, span.end, parent, lchild, rchild, w);
            } else {
              alreadyAdded = true; // already expanded a better node here
            }
        }

        if(!alreadyAdded) {
          val child = top.parent;
          val span = top.span;
          val score = top.score;
          if(span.start == 0 && span.end == s.length + 1 && child == root) {
            // we found our answer.
            break;
          }
          // otherwise, expand all related edges.

          val leftChildRules = grammar.binaryRulesByIndexedLeftChild(child);
          for {
            (rchild,parentScores) <- leftChildRules
            end <- span.end until (s.length+1)
            rchildScore = chart.labelScore(span.end,end,rchild)
            if rchildScore != Double.NegativeInfinity
            (parent,ruleScore) <- parentScores.activeElements
            if chart.labelScore(span.start, end, parent).isNegInfinity
          } {
            val totalScore = ruleScore + score + rchildScore;
            agenda += BinaryItem(parent, child, rchild, Span(span.start,end),span.end,totalScore);
          }

          val rightChildRules = grammar.binaryRulesByIndexedRightChild(child);
          for {
            (lchild,parentScores) <- rightChildRules
            start <- 0 until span.start
            lchildScore = chart.labelScore(start,span.start,lchild)
            if lchildScore != Double.NegativeInfinity
            (parent,ruleScore) <- parentScores.activeElements
            if chart.labelScore(start, span.end, parent).isNegInfinity
          } {
            val totalScore = ruleScore + score + lchildScore;
            agenda += BinaryItem(parent, lchild, child, Span(start,span.end),span.start,totalScore);
          }

          val unaryRules = grammar.unaryRulesByIndexedChild(child);
          for {
            (parent,ruleScore) <- unaryRules
            if chart.labelScore(span.start, span.end, parent).isNegInfinity
          } {
            val totalScore = ruleScore + score;
            agenda += UnaryItem(parent, child, span, totalScore);
          }

        }


      }
    }

    // TODO: graph outside scores (?)
    val bestParse = new ViterbiDecoder[L]{}.extractBestParse(root, grammar, chart, null);
    val c = DoubleCounter[Tree[L]]();
    c(bestParse) = chart.labelScore(0, s.length, root);
    c;
  }

}
