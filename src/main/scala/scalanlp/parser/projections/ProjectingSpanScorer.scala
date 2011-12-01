package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
class ProjectingSpanScorer[C,F](proj: GrammarProjections[C,F],
                                val scorer: SpanScorer[C],
                                downWeight: Boolean=true) extends SpanScorer[F] {

  private def labelProjections = proj.labels
  private def ruleProjections = proj.rules

  private val labelAdjustments = new Array[Double](labelProjections.coarseIndex.size);
  if(downWeight)
    for(c <- 0 until labelProjections.coarseIndex.size) {
      labelAdjustments(c) = math.log(labelProjections.refinementsOf(c).length);
    }

  private val ruleAdjustments = new Array[Double](ruleProjections.coarseIndex.size);
  if(downWeight)
    for(c <- 0 until ruleProjections.coarseIndex.size) {
      ruleAdjustments(c) = math.log(ruleProjections.refinementsOf(c).length);
    }

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    val pTag = labelProjections.project(tag)
    val raw = scorer.scoreSpan(begin,end, pTag)
    if(raw != Double.NegativeInfinity) raw - labelAdjustments(pTag);
    else raw
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val pRule = ruleProjections.project(rule)
    scorer.scoreUnaryRule(begin,end,pRule) - ruleAdjustments(pRule);
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val pRule = ruleProjections.project(rule)
    scorer.scoreBinaryRule(begin,split, end,pRule) - ruleAdjustments(pRule);
  }
}
