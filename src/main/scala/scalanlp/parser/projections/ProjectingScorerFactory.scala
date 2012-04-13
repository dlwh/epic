package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
class ProjectingScorerFactory[L, W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                                    val projector: ChartProjector[L, W]) extends DerivationScorer.Factory[L, W] {


  def grammar = parser.grammar.grammar

  def lexicon = parser.grammar.lexicon

  def specialize(words: Seq[W]) = {
    mkSpanScorer(words)
  }

  def mkSpanScorer(s: Seq[W], goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags) = {
    val charts = parser.charts(s)

    projector.buildSpanScorer(charts, goldTagPolicy)
  }

}

