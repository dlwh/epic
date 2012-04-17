package scalanlp.parser
package projections


/**
 * Projects a chart to a span scorer
 * @author dlwh
 */
trait ChartProjector[L, W] {
  type MyScorer <: UnrefinedDerivationScorer[L, W]
  protected def threshold:Double
  protected def createSpanScorer(charts: Marginal[L, W],
                                 ruleData: AnchoredRuleProjector.AnchoredData,
                                 sentProb: Double):MyScorer

  private def proj = new AnchoredRuleProjector(threshold)

  def buildSpanScorer(charts: Marginal[L, W],
                      goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):MyScorer = {

    val ruleData = proj.projectRulePosteriors(charts, goldTagPolicy)
    createSpanScorer(charts, ruleData, charts.partition)
  }
}
