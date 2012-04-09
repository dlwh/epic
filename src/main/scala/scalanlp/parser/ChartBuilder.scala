package scalanlp.parser

import scalanlp.util.TypeTags.{ID, tag}
import scalanlp.parser.ParseChart.logProb

/**
 *
 * @author dlwh
 */
trait ChartBuilder[+Chart[X]<:ParseChart[X], L, W] {

  def root: L = grammar.root
  def rootIndex: ID[L] = tag[L](grammar.labelIndex(root))
  def grammar: WeightedGrammar[L, W]

  def charts(words: Seq[W]):ChartMarginal[Chart, L, W]

  def withCharts[Chart2[X]<:ParseChart[X]](prob: ParseChart.Factory[Chart2]): ChartBuilder[Chart2, L, W]
}

