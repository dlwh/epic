package scalanlp.parser
package projections

import scalanlp.util.TypeTags
import TypeTags._
import scalanlp.trees.Rule

/**
 * 
 * @author dlwh
 */
final class ProjectingSpanScorer[C,F](proj: GrammarRefinements[C,F],
                                val scorer: SpanScorer[C]) extends SpanScorer[F] {

  private def labelProjections = proj.labels
  private def ruleProjections = proj.rules

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    val pTag = labelProjections.project(tag)
    val raw = scorer.scoreSpan(begin,end, pTag)
    raw
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val pRule = ruleProjections.project(rule)
    scorer.scoreUnaryRule(begin,end,pRule)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val pRule = ruleProjections.project(rule)
    scorer.scoreBinaryRule(begin,split, end,pRule)
  }
}
