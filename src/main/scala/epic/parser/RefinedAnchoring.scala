package epic.parser
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import nak.inference.Factor
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints

/**
 * A RefinedAnchoring is a refined grammar that has been tuned to a particular sentence (if applicable).
 * It knows how to do two things: assign scores to rules and spans, and determine reachability of various refinements.
 *
 * It might be nice to consider a refined grammar that doesn't need sentence-specific tuning, but
 * that interferes with integrating lexicalization into the framework.
 *
 * @author dlwh
 */
trait RefinedAnchoring[L, W]  {
  def grammar: BaseGrammar[L]
  def lexicon: Lexicon[L, W]
  def words: IndexedSeq[W]


  def logPartition: Double = marginal.logPartition

  /**
   * Scores the indexed label rule with refinenemnt ref, when it occurs at (begin, end). Can be used for s, or for a
   * "bottom" label. Mainly used for s.
   */
  def scoreSpan(begin: Int, end: Int, label: Int, ref: Int):Double

  /**
   * Scores the indexed [[epic.trees.BinaryRule]] rule when it occurs at (begin, split, end)
   */
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int):Double

  /**
   * Scores the indexed [[epic.trees.UnaryRule]] rule when it occurs at (begin, end)
   */
  def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int):Double

  // factor methods
  /**
   * Computes the pointwise product of two grammars, augmenting
   * their refinement space to reflect this. If they share the same annotationTag,
   * (assuming it's non-negative) they will share their state space. (That is,
   * they will have the same annotations.)
   * @param other
   * @return
   */
  def *(other: RefinedAnchoring[L, W]):RefinedAnchoring[L,W] = {
    // hacky multimethod dispatch is hacky
    if (other eq null) this // ugh
    else if(other.isInstanceOf[CoreAnchoring.Identity[L, W]]) this
    else if(this.isInstanceOf[CoreAnchoring.Identity[L, W]]) other
    else new ProductRefinedAnchoring(this,other)
  }

  /**
   * Computes the pointwise division of two grammars, augmenting
   * their refinement space to reflect this. If they share the same annotationTag,
   * (assuming it's non-negative) they will share their state space. (That is,
   * they will have the same annotations.)
   * @param other
   * @return
   */
  def /(other: RefinedAnchoring[L, W]):RefinedAnchoring[L,W] = {
    if(other.eq(null) || other.isInstanceOf[CoreAnchoring.Identity[L, W]]) this
    else new ProductRefinedAnchoring(this,other,-1)
  }

  def marginal = AugmentedAnchoring.fromRefined(this).marginal

  def isConvergedTo(f: RefinedAnchoring[L, W], diff: Double):Boolean = {
    import scala.util.control.Breaks._
    var ok = false
    breakable {
      marginal visit new AnchoredVisitor[L] {
        def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
          val myScore = scoreBinaryRule(begin, split, end, rule, ref)
          val theirScore = f.scoreBinaryRule(begin, split, end, rule, ref)



          if(myScore != theirScore)  {
            if (theirScore.isInfinite || myScore.isInfinite) {
                ok = true
                break()
              }
            val df = (myScore - theirScore).abs / math.max(math.max(myScore,theirScore).abs,1E-4)
            if(df > diff) {
              ok = true
              break()
            }
          }

        }

        def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
          val myScore = scoreUnaryRule(begin, end, rule, ref)
          val theirScore = f.scoreUnaryRule(begin, end, rule, ref)
          assert(!myScore.isInfinite)


          if(myScore != theirScore)  {
            if (theirScore.isInfinite || myScore.isInfinite) {
              ok = true
              break()
            }
            val df = (myScore - theirScore).abs / math.max(math.max(myScore,theirScore).abs, 1E-4)
            if(df > diff) {
              ok = true
              break()
            }
          }

        }

        def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) = {
          val myScore = scoreSpan(begin, end, tag, ref)
          val theirScore = f.scoreSpan(begin, end, tag, ref)


          if(myScore != theirScore)  {
            if(theirScore.isInfinite || myScore.isInfinite) {
              ok = true
              break()
            }
            val df = (myScore - theirScore).abs / math.max(math.max(myScore,theirScore).abs, 1E-4)
            if(df > diff) {
              ok = true
              break()
            }
          }

        }

      }
    }
    !ok
  }


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

  def annotationTag: Int = -1

  // stuff related to reachability
  /**
   * For a given span, what refinements to the label are allowed?
   * Refinements in general are in the range (0, numValidRefinements). This
   * method may return a subset.
   * @return array of valid refinements. Don't modify!
   */
  def validLabelRefinements(begin: Int, end: Int, label: Int):Array[Int]

  def maxLabelRefinements: Int = (0 until grammar.labelIndex.size).map(numValidRefinements _).max

  def numValidRefinements(label: Int):Int

  def numValidRuleRefinements(rule: Int):Int



  /**
   * For a given span and the parent's refinement, what refinements to the rule are allowed?
   * @param rule
   * @param begin
   * @param end
   * @return
   */
  def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int):Array[Int]

  def validRuleRefinementsGivenParent(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int, parentRef: Int):Array[Int] = validRuleRefinementsGivenParent(begin, end, rule, parentRef)

  def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, childRef: Int):Array[Int]
  def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int):Array[Int]
  def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int):Array[Int]

  def leftChildRefinement(rule: Int, ruleRef: Int):Int
  def rightChildRefinement(rule: Int, ruleRef: Int):Int
  def parentRefinement(rule: Int, ruleRef: Int):Int
  def childRefinement(rule: Int, ruleRef: Int):Int

  /**
   * Returns the refined rule given parent and child refinements for a unary rule.
   * May return -1 if no such rule is allowed.
   * @param r rule index
   * @param refA parent index
   * @param refB child index
   * @return rule refinement id, or -1 if rule is not allowed with those refinements
   */
  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int):Int

  /**
   * Returns the refined rule given parent and child refinements for a unary rule.
   * May return -1 if no such rule is allowed.
   * @param r rule Index
   * @param refA parent index
   * @param refB left child index
   * @param refC right child index
   * @return rule refinement id, or -1 if rule is not allowed with those refinements
   */
  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int):Int

  def validCoarseRulesGivenParentRefinement(a: Int, refA: Int): Array[Int]

  def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int]
  def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int]
  def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int]
}

object RefinedAnchoring {
  def identity[L, W](grammar: BaseGrammar[L],
                     lexicon: Lexicon[L, W],
                     words: IndexedSeq[W]): RefinedAnchoring[L, W] = {
    LiftedCoreAnchoring(CoreAnchoring.identity[L, W](grammar, lexicon, words, ChartConstraints.noSparsity[L]))
  }
}


trait BinaryRuleRefinements {
  val numChildRefinements: Int
  def leftChildForChildRefinement: Int
}
