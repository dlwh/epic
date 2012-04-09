package scalanlp.parser

import scalanlp.util.TypeTags._
import scalanlp.trees.Rule
import scalanlp.util.TypeTags

/**
 * A grammar that creates a span scorer as the Specialization and parses with it.
 * @author dlwh
 */
class SpanScorerGrammar[L, W](val grammar: Grammar[L], factory: SpanScorer.Factory[L, W]) extends WeightedGrammar[L, W] {
  def specialize(words: Seq[W]):Specialization = {
    val scorer = factory.mkSpanScorer(words)
    new Spec(WeightedGrammar.oneOff(grammar, scorer).specialize(words))
  }
  
  private class Spec(spec: WeightedGrammar[L,W]#Specialization) extends super.Specialization {
    def words = spec.words

    def scoreSpan(begin: Int, end: Int, label: ID[L], ref: ID[Ref[L]]) = {
      spec.scoreSpan(begin, end, label, ref)
    }

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: ID[Rule[L]], ref: ID[RuleRef[L]]) = {
      spec.scoreBinaryRule(begin, split,  end, rule, ref)
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: ID[Rule[L]], ref: ID[RuleRef[L]]) = {
      spec.scoreUnaryRule(begin, end, rule, ref)
    }

    def validLabelRefinements(begin: Int, end: Int, label: ID[L]) = {
      spec.validLabelRefinements(begin, end, label)
    }

    def numValidRefinements(label: Int) = {
      spec.numValidRefinements(label)
    }

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: ID[Rule[L]], parentRef: ID[Ref[L]]) = {
      spec.validRuleRefinementsGivenParent(begin, end, rule, parentRef)
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: ID[Rule[L]], childRef: ID[Ref[L]]) = {
      spec.validUnaryRuleRefinementsGivenChild(begin, end, rule, childRef)
    }

    def validTagsFor(pos: Int) = {
      spec.validTagsFor(pos)
    }

    def leftChildRefinement(rule: ID[Rule[L]], ruleRef: ID[RuleRef[L]]) = {
      spec.leftChildRefinement(rule, ruleRef)
    }

    def rightChildRefinement(rule: ID[Rule[L]], ruleRef: ID[RuleRef[L]]) = {
      spec.rightChildRefinement(rule, ruleRef)
    }

    def parentRefinement(rule: ID[Rule[L]], ruleRef: ID[RuleRef[L]]) = {
      spec.parentRefinement(rule, ruleRef)
    }

    def childRefinement(rule: ID[Rule[L]], ruleRef: ID[RuleRef[L]]) = {
      spec.childRefinement(rule, ruleRef)
    }

    def ruleRefinementFromRefinements(r: ID[Rule[L]], refA: ID[Ref[L]], refB: ID[Ref[L]], refC: ID[Ref[L]]) = {
      spec.ruleRefinementFromRefinements(r, refA, refB, refC)
    }

    def ruleRefinementFromRefinements(r: ID[Rule[L]], refA: ID[Ref[L]], refB: ID[Ref[L]]) = {
      spec.ruleRefinementFromRefinements(r, refA, refB)
    }
  }
}
