package scalanlp.parser.hypergraph

import scalanlp.parser.Lexicon
import scalanlp.parser.ParseChart
import scalanlp.parser.Parser
import scala.collection.mutable.PriorityQueue
import scalanlp.collection.mutable.SparseArray
import scalanlp.collection.mutable.TriangularArray
import scalanlp.counters.Counters.DoubleCounter
import scalanlp.parser.BinaryRule
import scalanlp.parser.Grammar
import scalanlp.parser.Rule
import scalanlp.parser.UnaryRule
import scalanlp.trees.BinarizedTree
import scalanlp.trees.BinaryTree
import scalanlp.trees.NullaryTree
import scalanlp.trees.Span;import scalanlp.trees.Tree
import scalanlp.trees.UnaryTree
import scalanlp.util.IntBloomFilter
import scala.util.control.Breaks._;
import scala.util.control.Breaks;


class GraphParser[L,W](root: L, lexicon: Lexicon[L,W], grammar: Grammar[L]) extends Parser[L,W] {

  sealed trait Item {
    def score: Double
    def span: Span
    def zero: Item
    def parent: Int
  }
  case class UnaryItem(parent: Int, child: Int, span: Span, score: Double) extends Item {
    def zero = copy(score = 0.0);
  }
  case class BinaryItem(parent: Int, leftChild: Int, rightChild: Int, span: Span, split: Int, score: Double) extends Item {
    def zero = copy(score = 0.0);
  }
  case class LexicalItem(l: Int, loc: Int, score: Double) extends Item {
    def zero = copy(score = 0.0);
    def span = new Span(loc,loc+1);
  }

  implicit val ordering: Ordering[Item] = Ordering.Double.on(_.score);

  val continues = new Breaks;

  def scores(s: Seq[W]) = {
    // make the chart:
    val chart = new ParseChart(grammar,s.length);
    val agenda = new PriorityQueue[Item];
    // elements that reach a particular span

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
        top match {
          case LexicalItem(parent, i, w) =>
            val span = top.span;
            if(chart.labelScore(span.start, span.end, parent).isInfinite) {
              chart.enterTerm(span.start, span.end, parent, w);
            } else {
              break; // already expanded a better node here
            }
          case UnaryItem(parent, child, span, w) =>
            if(chart.labelScore(span.start, span.end, parent).isInfinite) {
              chart.enterUnary(span.start, span.end, parent, child, w)
            } else {
              break; // already expanded a better node here
            }
          case BinaryItem(parent, lchild, rchild, span, split, w) =>
            if(chart.labelScore(span.start, span.end, parent).isInfinite) {
              chart.enterBinary(span.start, split, span.end, parent, lchild, rchild, w);
            } else {
              break; // already expanded a better node here
            }
        }

        val parent = top.parent;
        val span = top.span;
        val score = top.score;
        if(span.start == 0 && span.end == s.length + 1 && parent == root) {
          // we found our answer.
          break;
        }
        // otherwise, expand all related edges.


      }
    }

    val bestParse = chart.buildTree(0, s.length, grammar.index(root));
    val c = DoubleCounter[Tree[L]]();
    c(bestParse) = chart.labelScore(0, s.length, root);
    c;
  }

}
