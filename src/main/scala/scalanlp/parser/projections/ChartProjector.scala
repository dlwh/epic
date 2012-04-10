package scalanlp.parser
package projections


/**
 * Projects a chart to a span scorer
 * @author dlwh
 */
trait ChartProjector[L, W] {
  type MyScorer <: SpanScorer[L]
  protected def threshold:Double
  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double):MyScorer

  private def proj = new AnchoredRuleProjector(threshold)

  def buildSpanScorer(charts: Marginal[L, W],
                      goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):MyScorer = {

    val ruleData = proj.projectRulePosteriors(charts, goldTagPolicy)
    createSpanScorer(ruleData, charts.partition)
  }
}
