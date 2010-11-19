package scalanlp.parser
package projections

import scalanlp.collection.mutable.SparseArray
import scalala.tensor.counters.Counters.DoubleCounter
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.Vector
import scalanlp.util.Index;

import ParseChart._;
import ChartBuilder._;
import InsideOutside._;

/**
 * Project Charts from a Fine grammar to marginals in a finer grammar, returning a SpanFilter.
 * TODO: probably give direct access to marginals
 * @author dlwh
 */
class ChartMarginals[C,L,W](val parser: ChartBuilder[LogProbabilityParseChart,L,W], coarseIndex: Index[C], proj: L=>C) {
  def this(root: L, g: Grammar[L], lexicon: Lexicon[L,W], coarseIndex: Index[C], proj: L=>C)  = {
    this(new CKYChartBuilder[ParseChart.LogProbabilityParseChart,L,W](root,lexicon,g,logProb), coarseIndex, proj);
  }

  def grammar = parser.grammar;
  def lexicon = parser.lexicon;
  def root = parser.root;

  private val indexer = new ProjectionIndexer[C,L](coarseIndex, grammar.index,  proj);

  def projectParse(words: Seq[W], validSpan: SpanScorer =defaultScorer):SpanScorer = {
    val inside = parser.buildInsideChart(words, validSpan);
    val outside = parser.buildOutsideChart(inside, validSpan);
    val totalProb = inside.labelScore(0, words.length, root);

    projectCharts(inside,outside,totalProb)

  }

  def projectCharts(inside: LogProbabilityParseChart[L],
                    outside: LogProbabilityParseChart[L],
                    totalProb: Double): SpanScorer = {
    val compositeChart = ParseChart.logProb.apply(grammar,inside.length);
    // handle binary rules
    for{
      span <- 1 to inside.length
      begin <- 0 to (inside.length - span);
      end = begin + span
      (a,insideScore) <- inside.enteredLabelScores(begin,end)
    } {
      val outsideScore = outside.labelScore(begin,end,a);
      if(outsideScore != Double.NegativeInfinity && insideScore != Double.NegativeInfinity) {
        compositeChart.enter(begin,end, indexer(a), outsideScore + insideScore - totalProb);
      }
    }

    new SpanScorer {
      def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
        compositeChart.labelScore(begin,end,parent);
      }

      def scoreLexical(begin: Int, end: Int, tag: Int) = {
        compositeChart.labelScore(begin,end,tag);
      }

      def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
        compositeChart.labelScore(begin,end,parent);
      }
    }
  }
}

