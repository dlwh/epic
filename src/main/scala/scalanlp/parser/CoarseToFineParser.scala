package scalanlp.parser

import scalala.tensor.counters.Counters.DoubleCounter
import scalanlp.trees.Tree

import ChartParser._;

class CoarseToFineParser[C,F,W](coarseParser: ChartParser[C,W],
                                proj: F=>C,
                                val root: F,
                                val lexicon: Lexicon[F,W],
                                val grammar: Grammar[F],
                                threshold:Double = -100) extends ChartParser[F,W] {

  private val indexedProjections = grammar.fillArray(-1);
  for( (l,idx) <- grammar.index.zipWithIndex) {
    indexedProjections(idx) = coarseParser.grammar.index(proj(l));
  }
  private val coarseRootIndex = coarseParser.grammar.index(proj(root));

  private val fineParser = new CKYParser(root,lexicon,grammar);

  def buildInsideChart[Chart[X]<:ParseChart[X]](s: Seq[W],
                                                chart: ParseChart.Factory[Chart] = ParseChart.viterbi,
                                                validSpan: SpanFilter = defaultFilter):Chart[F] = {
    val coarseInside = coarseParser.buildInsideChart(s,chart, validSpan)
    val coarseOutside = coarseParser.buildOutsideChart(coarseInside, chart, validSpan);

    val sentProb = coarseInside(0,s.length,coarseRootIndex);

    def spanFilter(begin: Int, end: Int, label: Int) = (
      (coarseInside(begin,end,indexedProjections(label)) + coarseOutside(begin,end,indexedProjections(label)) - sentProb > threshold)
      && defaultFilter(begin,end,label)
    );

    fineParser.buildInsideChart(s,chart,spanFilter _);
  }

    /**
   * Given an inside chart, fills the passed-in outside parse chart with inside scores.
   */
  def buildOutsideChart[Chart[X]<:ParseChart[X]](inside: ParseChart[F],
                        chartFactory: ParseChart.Factory[Chart] = ParseChart.viterbi,
                        validSpan: SpanFilter = defaultFilter):Chart[F] = {
    fineParser.buildOutsideChart(inside, chartFactory,validSpan);
  }
  
}
