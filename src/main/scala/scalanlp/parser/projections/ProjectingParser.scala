package scalanlp.parser
package projections

import scalanlp.util.Index;
/**
 * A parser that takes a fine grammar and produces a chart, then projects that chart
 * somehow into a coarse tree. Currently, it's Viterbi.
 * @author dlwh
 */
object ProjectingParser {
  def apply[C,F,W](builder: ChartBuilder[ParseChart,F,W], coarseGrammar: Grammar[C], proj: F=>C) = {
    val indexedProjections = GrammarProjections(coarseGrammar,builder.grammar,proj)
    val decoder = new ViterbiDecoder[C,F,W](indexedProjections.labels);
    new SimpleChartParser(builder, decoder, indexedProjections);
  }
}