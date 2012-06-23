package epic.parser
package projections


/**
 * Projects a chart to a span anchoring
 * @author dlwh
 */
trait ChartProjector[L, W] {
  type MyAnchoring <: CoreAnchoring[L, W]
  protected def threshold:Double
  protected def createAnchoring(charts: Marginal[L, W],
                                 ruleData: AnchoredRuleProjector.AnchoredData,
                                 sentProb: Double):MyAnchoring

  private def proj = new AnchoredRuleProjector(threshold)

  def project(charts: Marginal[L, W],
              goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):MyAnchoring = {

    val ruleData = proj.projectRulePosteriors(charts, goldTagPolicy)
    createAnchoring(charts, ruleData, charts.partition)
  }
}
