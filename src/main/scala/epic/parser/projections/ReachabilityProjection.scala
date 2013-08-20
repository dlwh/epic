package epic.parser.projections

import epic.constraints.ChartConstraints
import epic.trees.{TreeInstance, BinarizedTree}
import epic.parser._
import epic.lexicon.{TagScorer, Lexicon}
import breeze.numerics.I
import com.typesafe.scalalogging.log4j.Logging
import scala.collection.{GenTraversableLike, GenTraversable, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom
import epic.util.{SafeLogging, CacheBroker}

/**
 * Finds the best tree (relative to the gold tree) s.t. it's reacheable given the current anchoring.
 * Best is measured as number of correct labeled spans, as usual.
 * @author dlwh
 */
class ReachabilityProjection[L, L2, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W], refinedGrammar: SimpleRefinedGrammar[L, L2, W]) extends SafeLogging {
  private val cache = CacheBroker().make[IndexedSeq[W], BinarizedTree[L2]]("ReachabilityProjection")

  private var problems  = 0
  private var total = 0


  private def refinements = refinedGrammar.refinements

  def forTree(tree: BinarizedTree[L2],
              words: IndexedSeq[W],
              constraints: ChartConstraints[L]) = try {
    val projectedTree: BinarizedTree[L] = tree.map(refinements.labels.project)
    cache.getOrElseUpdate(words, {
      val treeconstraints = ChartConstraints.fromTree(grammar.labelIndex, projectedTree)
      if(constraints.top.containsAll(treeconstraints.top) && constraints.bot.containsAll(treeconstraints.bot)) {
        synchronized(total += 1)
        tree
      } else {
        val w = words
        val marg = AugmentedAnchoring(makeGoldPromotingAnchoring(w, tree, treeconstraints), constraints).maxMarginal

        val closest: BinarizedTree[(L, Int)] = new ViterbiDecoder[L,W]().extractMaxDerivationParse(marg)

        logger.warn {
          val stats = new ParseEval(Set.empty[L]).apply(closest.map(_._1), projectedTree)
          val ratio =  synchronized{problems += 1; total += 1; problems * 1.0 / total}
          f"Gold tree for $words not reachable. Best has score: $stats. $ratio%.2f are bad so far. "
        }

        val globalizedClosest: BinarizedTree[L2] = closest.map({refinements.labels.globalize(_:L, _:Int)}.tupled)
        globalizedClosest
      }
    })
  } catch {
    case ex: Exception => throw new RuntimeException(s"while handling projectability for $tree $words", ex)
  }


  def makeGoldPromotingAnchoring(w: IndexedSeq[W],
                                 tree: BinarizedTree[L2],
                                 treeconstraints: ChartConstraints[L]): RefinedAnchoring[L, W] = {
    val correctRefinedSpans = GoldTagPolicy.goldTreeForcing(tree.map(refinements.labels.fineIndex))
    new RefinedAnchoring[L, W] {
      def words: IndexedSeq[W] = w

      val basic = refinedGrammar.anchor(w)

      def grammar: BaseGrammar[L] = ReachabilityProjection.this.grammar

      def lexicon = ReachabilityProjection.this.lexicon

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = 0.0

      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
        val top = grammar.parent(rule)
        val isAllowed = treeconstraints.top.isAllowedLabeledSpan(begin, end, top)
        val isRightRefined = isAllowed && {
          val rr = refinements.rules.globalize(rule, ref)
          val theRefinedRule = refinements.rules.fineIndex.get(rr)
          val refTop = refinements.labels.fineIndex(theRefinedRule.parent)
          correctRefinedSpans.isGoldTopTag(begin, end, refTop)
        }
        20 * I(isAllowed) + I(isRightRefined)
      }

      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int): Double = {
        val globalized = refinements.labels.globalize(tag, ref)
        20 * I(treeconstraints.bot.isAllowedLabeledSpan(begin, end, tag)) +
          I(correctRefinedSpans.isGoldBotTag(begin, end, globalized))
      }

      def validLabelRefinements(begin: Int, end: Int, label: Int): Array[Int] = refinements.labels.refinementsOf(label)

      def numValidRefinements(label: Int): Int = refinements.labels.refinementsOf(label).length

      def numValidRuleRefinements(rule: Int): Int = refinements.rules.refinementsOf(rule).length

      def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int): Array[Int] = {
        basic.validRuleRefinementsGivenParent(begin, end, rule, parentRef)
      }

      def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, childRef: Int): Array[Int] = {
        basic.validRuleRefinementsGivenRightChild(begin, split, completionBegin, completionEnd, rule, childRef)
      }

      def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
        basic.validRuleRefinementsGivenRightChild(completionBegin, completionEnd, split, end, rule, childRef)
      }

      def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
        basic.validUnaryRuleRefinementsGivenChild(begin, end, rule, childRef)
      }

      def leftChildRefinement(rule: Int, ruleRef: Int): Int = basic.leftChildRefinement(rule, ruleRef)

      def rightChildRefinement(rule: Int, ruleRef: Int): Int = basic.rightChildRefinement(rule, ruleRef)

      def parentRefinement(rule: Int, ruleRef: Int): Int = basic.parentRefinement(rule, ruleRef)

      def childRefinement(rule: Int, ruleRef: Int): Int = basic.childRefinement(rule, ruleRef)

      def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int): Int = basic.ruleRefinementFromRefinements(r, refA, refB)

      def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int): Int = basic.ruleRefinementFromRefinements(r, refA, refB, refC)

      def validCoarseRulesGivenParentRefinement(a: Int, refA: Int): Array[Int] = basic.validCoarseRulesGivenParentRefinement(a, refA)

      def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = basic.validParentRefinementsGivenRule(begin, splitBegin, splitEnd, end, rule)

      def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int] = basic.validLeftChildRefinementsGivenRule(begin, end, completionBegin, completionEnd, rule)

      def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int] = basic.validRightChildRefinementsGivenRule(completionBegin, completionEnd, begin, end, rule)
    }
  }
}

