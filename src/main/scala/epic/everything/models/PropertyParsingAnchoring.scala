package epic.everything.models

import epic.parser.{Lexicon, BaseGrammar, RefinedAnchoring}
import epic.parser.models.LexGrammar
import epic.trees.Span
import breeze.collection.mutable.TriangularArray

/**
 *
 * @author dlwh
 */
final class PropertyParsingAnchoring[L, W](val lexGrammar: LexGrammar[L, W],
                                           val words: IndexedSeq[W],
                                           val beliefs: SentenceBeliefs) extends RefinedAnchoring[L, W] {

  /**
   * The annotationTag controls if two grammars are over the same refinements.
   * If they are, then * and / can be much faster.
   *
   * Note that 0 is reserved for unrefined anchorings, and -1 never matches other tags.
   *
   * Reserved:
   * 1 - Lexicalized Parsers with no symbol or rule annotation
   *
   * 0's will be optimized
   */
  override def annotationTag: Int = 1

  val anchoring = lexGrammar.anchor(words)
  private val notConstituent = grammar.labelIndex.size

  def length = words.length
  def grammar: BaseGrammar[L] = lexGrammar.grammar

  def lexicon: Lexicon[L, W] = lexGrammar.lexicon

  def scoreSpan(begin: Int, end: Int, label: Int, ref: Int): Double = {

    var baseScore = anchoring.scoreSpan(begin, end, label, ref)
    if (begin == 0 && end == length) ( // root, get the length
      baseScore += math.log(beliefs.wordBeliefs(ref).governor(length)
       * beliefs.spanBeliefs(begin, end).governor(length)
//       * beliefs.wordBeliefs(ref).span(TriangularArray.index(begin,end))
        / beliefs.spanBeliefs(begin, end).governor(length+1))
     )

     if(begin + 1 == end) {
       baseScore + math.log(beliefs.wordBeliefs(begin).tag(label))
     } else {
       baseScore
     }
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
    val head = anchoring.headIndex(ref)
    val dep = anchoring.depIndex(ref)
    val depScore = beliefs.wordBeliefs(dep).governor(head)

    if (lexGrammar.isRightRule(rule)) {
      val sGovScore = beliefs.spans(begin, split).governor(head)
      val notASpan = beliefs.spanBeliefs(begin, split).governor(length + 1)
      if(depScore == 0.0 || sGovScore == 0.0 || notASpan >= 1.0) {
        Double.NegativeInfinity
      } else {
        val baseScore = anchoring.scoreBinaryRule(begin, split, end, rule, ref)  +
          math.log(depScore * sGovScore / notASpan)
        baseScore
      }
      // head on the right
    } else {
     anchoring.scoreBinaryRule(begin, split, end, rule, ref) +
       math.log(depScore * beliefs.spans(split, end).governor(head) / beliefs.spanBeliefs(split, end).governor(length+1))
    }
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
    val parent = grammar.parent(rule)
   anchoring.scoreUnaryRule(begin, end, rule, ref) +  math.log(beliefs.spanBeliefs(begin, end).label(parent) / beliefs.spanBeliefs(begin, end).label(notConstituent))
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
