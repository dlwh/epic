package epic.everything.models

import epic.parser.{Lexicon, BaseGrammar, RefinedAnchoring}
import epic.parser.models.LexGrammar
import epic.trees.Span

/**
 *
 * @author dlwh
 */
final class PropertyParsingAnchoring[L, W](val lexGrammar: LexGrammar[L, W],
                                           notConstituent: L,
                                           val words: IndexedSeq[W],
                                           val beliefs: SentenceBeliefs) extends RefinedAnchoring[L, W] {
  val anchoring = lexGrammar.anchor(words)
  private val nc = grammar.labelIndex(notConstituent)

  def length = words.length
  def grammar: BaseGrammar[L] = lexGrammar.grammar

  def lexicon: Lexicon[L, W] = lexGrammar.lexicon

  def scoreSpan(begin: Int, end: Int, label: Int, ref: Int): Double = {

    val baseScore = if (begin == 0 && end == length) ( // root, get the length
       math.log(beliefs.wordBeliefs(ref).governor(length)
       * beliefs.spanBeliefs(begin, end).governor(length)
       / beliefs.spanBeliefs(begin, end).governor(length+1))
     ) else 0.0

     if(begin + 1 == end) {
       baseScore + math.log(beliefs.words(begin).tag(label))
     } else {
       baseScore
     }
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
    val head = anchoring.headIndex(ref)
    val dep = anchoring.depIndex(ref)
    val score = beliefs.words(dep).governor(head)
    if (lexGrammar.isRightRule(rule)) { // head on the right
      math.log(score * beliefs.spans(begin, split).governor(head) / beliefs.spanBeliefs(begin, split).governor(length+1))
    } else {
      math.log(score * beliefs.spans(split, end).governor(head) / beliefs.spanBeliefs(split, end).governor(length+1))
    }
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
    val parent = grammar.parent(rule)
    math.log(beliefs.spanBeliefs(begin, end).label(parent) / beliefs.spanBeliefs(begin, end).label(nc))
  }

  def validLabelRefinements(begin: Int, end: Int, label: Int): Array[Int] = anchoring.validLabelRefinements(begin, end, label)

  def numValidRefinements(label: Int): Int = anchoring.numValidRefinements(label)

  def numValidRuleRefinements(rule: Int): Int = anchoring.numValidRuleRefinements(rule)

  def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int): Array[Int] = {
    anchoring.validRuleRefinementsGivenParent(begin, end, rule, parentRef)
  }

  def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
    anchoring.validUnaryRuleRefinementsGivenChild(begin, end, rule, childRef)
  }

  def leftChildRefinement(rule: Int, ruleRef: Int): Int = {
    anchoring.leftChildRefinement(rule, ruleRef)
  }

  def rightChildRefinement(rule: Int, ruleRef: Int): Int = {
    anchoring.rightChildRefinement(rule, ruleRef)
  }

  def parentRefinement(rule: Int, ruleRef: Int): Int = {
    anchoring.parentRefinement(rule, ruleRef)
  }

  def childRefinement(rule: Int, ruleRef: Int): Int = {
    anchoring.childRefinement(rule, ruleRef)
  }


  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int): Int = {
    anchoring.ruleRefinementFromRefinements(r, refA, refB)
  }

  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int): Int = {
    anchoring.ruleRefinementFromRefinements(r, refA, refB, refC)
  }


  def validCoarseRulesGivenParentRefinement(a: Int, refA: Int): Array[Int] = {
    anchoring.validCoarseRulesGivenParentRefinement(a, refA)
  }
}
