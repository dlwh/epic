package scalanlp.parser
package projections

import scalanlp.trees.BinarizedTree
/**
 * 
 * @author dlwh
 */
abstract class ChartDrivenScorerFactory[C,L,W](coarseGrammar: Grammar[C],
                                               parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                               indexedProjections: GrammarProjections[C,L],
                                               pruningThreshold: Double = -7) extends SpanScorer.Factory[C,L,W] {

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity) = {
    val coarseRootIndex = parser.grammar.labelIndex(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);

    val sentProb = inside.top.labelScore(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(inside,outside,sentProb,scorer);

    chartScorer;
  }

  def mkSpanScorerWithTree(s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity, tree: BinarizedTree[C]=null) = {
    val coarseRootIndex = parser.grammar.labelIndex(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);

    val sentProb = inside.top.labelScore(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(inside,outside,sentProb,scorer,tree);

    chartScorer;
  }

  val proj = new AnchoredRuleProjector[C,L,W](parser, indexedProjections);

  type MyScorer <:SpanScorer[C]


  def buildSpanScorer(inside: ParseChart[L],
                      outside: ParseChart[L],
                      sentProb: Double,
                      scorer: SpanScorer[L]=SpanScorer.identity[L],
                      tree: BinarizedTree[C] = null):MyScorer = {
    import AnchoredRuleProjector._;

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

