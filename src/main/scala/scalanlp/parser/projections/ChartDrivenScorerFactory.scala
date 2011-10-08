package scalanlp.parser
package projections

import scalanlp.trees.BinarizedTree
/**
 * 
 * @author dlwh
 */
abstract class ChartDrivenScorerFactory[C,L,W](coarseGrammar: Grammar[C],
                                               parser: SimpleChartParser[C,L,W],
                                               pruningThreshold: Double = -7) extends SpanScorer.Factory[C,L,W] {

  def indexedProjections = parser.projections

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity) = {
    val charts = parser.charts(s,scorer)

    val sentProb = charts.inside.top.labelScore(0,s.length,parser.root)
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(charts,sentProb);

    chartScorer;
  }

  def mkSpanScorerWithTree(s: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity, tree: BinarizedTree[C]=null) = {
 val charts = parser.charts(s,scorer)

    val sentProb = charts.inside.top.labelScore(0,s.length,parser.root)
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(charts,sentProb,tree);

    chartScorer;
  }

  val proj = new AnchoredRuleProjector[C,L,W](coarseGrammar, parser.builder.withCharts(ParseChart.logProb), indexedProjections);

  type MyScorer <:SpanScorer[C]


  def buildSpanScorer(charts: ChartPair[ParseChart,L],
                      sentProb: Double,
                      tree: BinarizedTree[C] = null):MyScorer = {
    import AnchoredRuleProjector._;
    import charts._

    val pruner: SpanScorer[C] = {
      if(tree == null) thresholdPruning(pruningThreshold)
      else {
        val gold = goldTreeForcing(coarseGrammar, tree.map(indexedProjections.labels.coarseIndex))
        SpanScorer.sum(gold,thresholdPruning(pruningThreshold))
      };
    }
    val ruleData = proj.projectRulePosteriors(inside,outside,sentProb,scorer,pruner);

    createSpanScorer(ruleData, sentProb);
  }

  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double):MyScorer;

}

