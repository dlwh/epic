package scalanlp.parser
package projections

import scalanlp.util.Index;
/**
 * A parser that takes a fine grammar and produces a chart, then projects that chart
 * somehow into a coarse tree. Currently, it's Viterbi.
 * @author dlwh
 */
object ProjectingParser {
  def apply[C,F,W](builder: ChartBuilder[ParseChart,F,W], coarseIndex: Index[C], proj: F=>C) = {
    val indexedProjections = ProjectionIndexer(coarseIndex,builder.grammar.index,proj);
    val decoder = new ViterbiDecoder[C,F,W](indexedProjections);
    new ChartParser(builder, decoder, indexedProjections);
  }
}