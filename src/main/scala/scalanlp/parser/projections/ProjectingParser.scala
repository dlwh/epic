package scalanlp.parser
package projections

import scalanlp.trees._
import scalanlp.util.Index;
import scalala.tensor.counters.Counters.DoubleCounter

/**
 * A parser that takes a fine grammar and produces a chart, then projects that chart
 * somehow into a coarse tree. Currently, it's Viterbi.
 * @author dlwh
 */
object ProjectingParser {
  def apply[C,F,W](builder: ChartBuilder[ParseChart,F,W], coarseIndex: Index[C], proj: F=>C) = {
    val indexedProjections = new ProjectionIndexer(coarseIndex,builder.grammar.index,proj);
    val decoder = new ViterbiDecoder(indexedProjections);
    new ChartParser(builder, decoder);
  }
}