package epic.parser

import projections.GrammarRefinements
import epic.trees.{BinaryRule, UnaryRule}
import collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1)
class SimpleRefinedGrammar[L, L2, W](val grammar: BaseGrammar[L],
                                     val lexicon: Lexicon[L, W],
                                     val refinements: GrammarRefinements[L, L2],
                                     val refinedGrammar: BaseGrammar[L2],
                                     ruleScoreArray: Array[Array[Double]],
                                     spanScoreArray: Array[Array[Double]],
                                     parentCompatibleRefinements: Array[Array[Array[Int]]],
                                     childCompatibleRefinements: Array[Array[Array[Int]]],
                                     tagScorer: TagScorer[L2, W]) extends RefinedGrammar[L, W] with Serializable {

  //
  private val coarseRulesGivenParentRefinement = Array.tabulate(grammar.labelIndex.size) { p =>
    // refinement -> rules
    val result = Array.fill(refinements.labels.refinementsOf(p).size)(ArrayBuffer[Int]())
    for(r <- grammar.indexedBinaryRulesWithParent(p); ref <- 0 until result.length) {
      if(parentCompatibleRefinements(r)(ref).nonEmpty) {
        result(ref) += r
      }
    }

    result.map(_.toArray)
  }

  def anchor(w: Seq[W]) = new RefinedAnchoring[L, W] {
    override def toString() = "SimpleAnchoring(...)"
    val grammar = SimpleRefinedGrammar.this.grammar
    val lexicon = SimpleRefinedGrammar.this.lexicon
    def words = w

    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
      val baseScore = if(begin + 1 == end) {
        val fullId = refinements.labels.globalize(label, ref)
        tagScorer.scoreTag(refinements.labels.fineIndex.get(fullId), words, begin)
      } else {
        0.0
      }
      baseScore + spanScoreArray(label)(ref)
    }

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      ruleScoreArray(rule)(ref)
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      ruleScoreArray(rule)(ref)
    }

    def validLabelRefinements(begin: Int, end: Int, label: Int) = {
      refinements.labels.localRefinements(label)
    }

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      parentCompatibleRefinements(rule)(parentRef)
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
      childCompatibleRefinements(rule)(childRef)
    }

    def leftChildRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.leftChild(refinedRuleId))
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.rightChild(refinedRuleId))
    }

    def parentRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.parent(refinedRuleId))
    }

    def childRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.child(refinedRuleId))
    }

    // TODO: make this not terminally slow!
    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
      val a = grammar.parent(r)
      val b = grammar.child(r)
      val a2 = refinements.labels.globalize(a, refA)
      val b2 = refinements.labels.globalize(b, refB)
      val rule = UnaryRule(refinements.labels.fineIndex.get(a2), refinements.labels.fineIndex.get(b2), grammar.chain(r))
      val refinedRuleIndex = refinements.rules.fineIndex(rule)
      if(refinedRuleIndex < 0) {
        -1
      } else {
        refinements.rules.localize(refinedRuleIndex)
      }
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
      val a = grammar.parent(r)
      val b = grammar.leftChild(r)
      val c = grammar.rightChild(r)
      val a2 = refinements.labels.globalize(a, refA)
      val b2 = refinements.labels.globalize(b, refB)
      val c2 = refinements.labels.globalize(c, refC)
      refinements.rules.localize(refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
        refinements.labels.fineIndex.get(b2),
        refinements.labels.fineIndex.get(c2)
      ))  )
    }

    def numValidRefinements(label: Int) = refinements.labels.refinementsOf(label).length
    def numValidRuleRefinements(rule: Int) = refinements.rules.refinementsOf(rule).length

    def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = coarseRulesGivenParentRefinement(a)(refA)
  }
}
