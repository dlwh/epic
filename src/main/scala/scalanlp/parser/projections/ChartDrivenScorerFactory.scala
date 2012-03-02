package scalanlp.parser
package projections

import scalanlp.trees.BinarizedTree
/**
 * 
 * @author dlwh
 */
abstract class ChartDrivenScorerFactory[C,L,W](coarseGrammar: Grammar[C],
                                               parser: SimpleChartParser[C,L,W],
                                               threshold: Double) extends SpanScorer.Factory[C,L,W] {

  def indexedProjections = parser.projections

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity, goldTag: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags) = {
    val charts = parser.charts(s,scorer)

    val sentProb = charts.inside.top.labelScore(0,s.length,parser.root)
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(charts, goldTag);

    chartScorer;
  }

  val proj = new AnchoredRuleProjector[C,L,W](coarseGrammar, parser.builder.withCharts(ParseChart.logProb), indexedProjections, threshold);

  type MyScorer <:SpanScorer[C]

  def buildSpanScorer(charts: ChartPair[ParseChart,L],
                      goldTagPolicy: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags[C]):MyScorer = {

    val ruleData = proj.projectRulePosteriors(charts,goldTagPolicy);

    createSpanScorer(ruleData, charts.partition);
  }

  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double):MyScorer;

}

