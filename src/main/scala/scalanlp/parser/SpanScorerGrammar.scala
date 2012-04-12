package scalanlp.parser

import scalanlp.util.TypeTags._
import scalanlp.trees.Rule
import scalanlp.util.TypeTags

/**
 * A grammar that creates a span scorer as the Specialization and parses with it.
 * @author dlwh
 */
class SpanScorerGrammar[L, W](val grammar: Grammar[L], val lexicon: Lexicon[L, W], factory: SpanScorer.Factory[L, W]) extends DerivationScorer.Factory[L, W] {
  def specialize(words: Seq[W]):Specialization = {
    val scorer = factory.mkSpanScorer(words)
    new Spec(DerivationScorerFactory.oneOff(grammar, lexicon, scorer).specialize(words), words)
  }
  
  private class Spec(spec: DerivationScorer[L, W], val words: Seq[W]) extends super.Specialization {
    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
      spec.scoreSpan(begin, end, label, ref)
    }

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      spec.scoreBinaryRule(begin, split,  end, rule, ref)
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      spec.scoreUnaryRule(begin, end, rule, ref)
    }

    def validLabelRefinements(begin: Int, end: Int, label: Int) = {
      spec.validLabelRefinements(begin, end, label)
    }

    def numValidRefinements(label: Int) = {
      spec.numValidRefinements(label)
    }

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      spec.validRuleRefinementsGivenParent(begin, end, rule, parentRef)
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
      spec.validUnaryRuleRefinementsGivenChild(begin, end, rule, childRef)
    }

    def leftChildRefinement(rule: Int, ruleRef: Int) = {
      spec.leftChildRefinement(rule, ruleRef)
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      spec.rightChildRefinement(rule, ruleRef)
    }

    def parentRefinement(rule: Int, ruleRef: Int) = {
      spec.parentRefinement(rule, ruleRef)
    }

    def childRefinement(rule: Int, ruleRef: Int) = {
      spec.childRefinement(rule, ruleRef)
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
      spec.ruleRefinementFromRefinements(r, refA, refB, refC)
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
      spec.ruleRefinementFromRefinements(r, refA, refB)
    }
  }
}
