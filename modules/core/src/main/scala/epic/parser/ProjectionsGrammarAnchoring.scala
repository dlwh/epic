package epic.parser

import epic.parser.projections.GrammarRefinements
import epic.trees.{BinaryRule, UnaryRule}

/**
 * TODO
 *
 * @author dlwh
 **/
trait ProjectionsGrammarAnchoring[L, L2, W] extends GrammarAnchoring[L, W] {
  def refinements: GrammarRefinements[L, L2]
  def refinedTopology: RuleTopology[L2]

  final def validLabelRefinements(begin: Int, end: Int, label: Int) = {
    refinements.labels.localRefinements(label)
  }

  final def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
    refinements.ruleRefinementsCompatibleWithParentRef(rule, parentRef)
  }

  final def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, childRef: Int): Array[Int] = {
    refinements.ruleRefinementsCompatibleWithLeftRef(rule, childRef)
  }

  final def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
    refinements.ruleRefinementsCompatibleWithRightRef(rule, childRef)
  }

  final def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
    refinements.ruleRefinementsCompatibleWithChildRef(rule, childRef)
  }

  final def leftChildRefinement(rule: Int, ruleRef: Int) = {
    val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
    refinements.labels.localize(refinedTopology.leftChild(refinedRuleId))
  }

  final def rightChildRefinement(rule: Int, ruleRef: Int) = {
    val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
    refinements.labels.localize(refinedTopology.rightChild(refinedRuleId))
  }

  final def parentRefinement(rule: Int, ruleRef: Int) = {
    refinements.parentRefinement(rule, ruleRef)
  }

  final def childRefinement(rule: Int, ruleRef: Int) = {
    val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
    refinements.labels.localize(refinedTopology.child(refinedRuleId))
  }

  // TODO: make this not terminally slow!
  final def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
    val a = topology.parent(r)
    val b = topology.child(r)
    val a2 = refinements.labels.globalize(a, refA)
    val b2 = refinements.labels.globalize(b, refB)
    val rule = UnaryRule(refinements.labels.fineIndex.get(a2), refinements.labels.fineIndex.get(b2), topology.chain(r))
    val refinedRuleIndex = refinements.rules.fineIndex(rule)
    if (refinedRuleIndex < 0) {
      -1
    } else {
      refinements.rules.localize(refinedRuleIndex)
    }
  }

  final def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
    val a = topology.parent(r)
    val b = topology.leftChild(r)
    val c = topology.rightChild(r)
    val a2 = refinements.labels.globalize(a, refA)
    val b2 = refinements.labels.globalize(b, refB)
    val c2 = refinements.labels.globalize(c, refC)
    val rule = BinaryRule(refinements.labels.fineIndex.get(a2),
      refinements.labels.fineIndex.get(b2),
      refinements.labels.fineIndex.get(c2)
    )
    val fi = refinements.rules.fineIndex(rule)
    if (fi < 0) throw new RuntimeException(s"No such rule: $rule")
    refinements.rules.localize(fi)
  }

  final def numValidRefinements(label: Int) = refinements.labels.refinementsOf(label).length
  final def numValidRuleRefinements(rule: Int) = refinements.rules.refinementsOf(rule).length

  final def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = {
    refinements.coarseRulesGivenParentRef(a, refA)
  }

  final def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
    refinements.parentRefinementsCompatibleWithRule(rule)
  }
  final def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int] = {
    refinements.leftChildRefinementsCompatibleWithRule(rule)
  }
  final def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int] = {
    refinements.rightChildRefinementsCompatibleWithRule(rule)
  }

}
