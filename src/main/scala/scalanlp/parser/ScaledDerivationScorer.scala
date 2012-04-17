package scalanlp.parser

/**
 * Adds score to the root .
 * @author dlwh
 */
class ScaledDerivationScorer[L, W](s: DerivationScorer[L, W], score: Double) extends DerivationScorer[L, W] {
  def grammar = s.grammar

  def lexicon = s.lexicon
  private val rootIndex = grammar.labelIndex(grammar.root)

  def words = s.words

  def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = s.scoreSpan(begin, end, label, ref)

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
    s.scoreBinaryRule(begin, split, end, rule, ref)
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
    if(grammar.parent(rule) == rootIndex) s.scoreUnaryRule(begin, end, rule, ref) + score
    else s.scoreUnaryRule(begin, end, rule, ref)
  }

  def validLabelRefinements(begin: Int, end: Int, label: Int) = {
    s.validLabelRefinements(begin, end, label)
  }

  def numValidRefinements(label: Int) = s.numValidRefinements(label)

  def numValidRuleRefinements(rule: Int) = s.numValidRuleRefinements(rule)

  def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
    s.validRuleRefinementsGivenParent(begin, end, rule, parentRef)
  }

  def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
    s.validUnaryRuleRefinementsGivenChild(begin, end, rule, childRef)
  }

  def leftChildRefinement(rule: Int, ruleRef: Int) = s.leftChildRefinement(rule, ruleRef)

  def rightChildRefinement(rule: Int, ruleRef: Int) = s.rightChildRefinement(rule, ruleRef)

  def parentRefinement(rule: Int, ruleRef: Int) = s.parentRefinement(rule, ruleRef)

  def childRefinement(rule: Int, ruleRef: Int) = s.childRefinement(rule, ruleRef)

  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = s.ruleRefinementFromRefinements(r, refA, refB)

  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = s.ruleRefinementFromRefinements(r, refA, refB, refC)
}
