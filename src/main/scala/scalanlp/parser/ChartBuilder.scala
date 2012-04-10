package scalanlp.parser

import scalanlp.util.TypeTags.{ID, tag}
import scalanlp.parser.ParseChart.logProb

/**
 *
 * @author dlwh
 */
trait ChartBuilder[+Chart[X]<:ParseChart[X], L, W] {

  def root: L = grammar.root
  def rootIndex: Int = tag[L](grammar.labelIndex(root))
  def grammar: DerivationScorer.Factory[L, W]

  def charts(words: Seq[W]):ChartMarginal[Chart, L, W]

  def withCharts[Chart2[X]<:ParseChart[X]](prob: ParseChart.Factory[Chart2]): ChartBuilder[Chart2, L, W]
}

object ChartBuilder {
  def apply[L, W](grammar: DerivationScorer.Factory[L, W]):ChartBuilder[ParseChart.LogProbabilityParseChart, L, W] = {
    apply(grammar, ParseChart.logProb)
  }

  def apply[Chart[X]<:ParseChart[X], L, W](grammar: DerivationScorer.Factory[L, W],
                                           factory: ParseChart.Factory[Chart]):ChartBuilder[Chart, L, W] = {
    new CKYChartBuilder(grammar, factory)
  }
}
