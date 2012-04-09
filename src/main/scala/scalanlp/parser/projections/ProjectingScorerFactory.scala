package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
class ProjectingScorerFactory[L, W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                                    val projector: ChartProjector[L, W]) extends SpanScorer.Factory[L, W] {
  type MyScorer = projector.MyScorer


  def mkSpanScorer(s: Seq[W], goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags) = {
    val charts = parser.charts(s)

    projector.buildSpanScorer(charts, goldTagPolicy)
  }

}

