package scalanlp.parser
package projections

import scalanlp.trees.BinarizedTree
import scalanlp.collection.mutable.TriangularArray
import collection.mutable.BitSet

/**
 * 
 * @author dlwh
 */
abstract class ChartDrivenScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                     indexedProjections: ProjectionIndexer[C,L],
                                     pruningThreshold: Double = -7) extends SpanScorer.Factory[W] {

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer = SpanScorer.identity) = {
    val coarseRootIndex = parser.grammar.index(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);

    val sentProb = inside.top.labelScore(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(inside,outside,sentProb,scorer);

    chartScorer;
  }

  val proj = new AnchoredRuleProjector[C,L,W](parser, indexedProjections);

  type MyScorer <:SpanScorer


  def buildSpanScorer(inside: ParseChart[L],
                      outside: ParseChart[L],
                      sentProb: Double,
                      scorer: SpanScorer=SpanScorer.identity,
                      tree: BinarizedTree[C] = null):MyScorer = {
    import AnchoredRuleProjector._;

    val pruner = {
      if(tree == null) thresholdPruning(pruningThreshold)
      else goldTreeForcing(tree.map(indexedProjections.coarseIndex), thresholdPruning(pruningThreshold));
    }
    val ruleData = proj.projectRulePosteriors(inside,outside,sentProb,scorer,pruner);

    createSpanScorer(ruleData, sentProb);
  }

  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double):MyScorer;

}

