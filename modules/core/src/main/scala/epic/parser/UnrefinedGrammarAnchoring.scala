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

import epic.inference.Factor
import epic.lexicon.Lexicon
import epic.constraints.{TagConstraints, ChartConstraints}
import breeze.numerics.logI

/**
 * [[epic.parser.UnrefinedGrammarAnchoring]] score rules and labels in a particular context
 * without needed extra "refined" categories. That is, an anchoring can
 * score x-bar spans in a particular context.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait UnrefinedGrammarAnchoring[L, W] extends GrammarAnchoring[L, W] with Factor[UnrefinedGrammarAnchoring[L, W]] {
  def topology: RuleTopology[L]
  def lexicon: Lexicon[L, W]
  def words: IndexedSeq[W]

//  def sparsityPattern = ChartConstraints.noSparsity[L]
  def addConstraints(cs: ChartConstraints[L]):UnrefinedGrammarAnchoring[L, W]

  /**
   * Scores the indexed [[epic.trees.BinaryRule]] rule when it occurs at (begin,split,end)
   */
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double

  /**
   * Scores the indexed [[epic.trees.UnaryRule]] rule when it occurs at (begin,end)
   */
  def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double

  /**
   * Scores the indexed label rule when it occurs at (begin,end). Can be used for tags, or for a
   * "bottom" label. Typically it is used to filter out impossible rules (using Double.NegativeInfinity)
   */
  def scoreSpan(begin: Int, end: Int, tag: Int): Double

  // Factor stuff
  /**
   * Computes the point-wise product of this grammar with some other grammar.
   *
   * Note that scores are in log space, so we actually sum scores.
   * @param other
   * @return
   */
  override def *(other: UnrefinedGrammarAnchoring[L, W]): UnrefinedGrammarAnchoring[L, W] = {
    // hacky multimethod dispatch is hacky
    if (other eq null) this // ugh
    else if (other.isInstanceOf[UnrefinedGrammarAnchoring.Identity[L, W]]) this.addConstraints(other.sparsityPattern)
    else if (this.isInstanceOf[UnrefinedGrammarAnchoring.Identity[L, W]]) other.addConstraints(this.sparsityPattern)
    else new ProductUnrefinedGrammarAnchoring(this,other)
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
  override def annotationTag: Int = 0

  /**
   * Computes the point-wise division of this grammar with some other grammar.
   *
   * Note that scores are in log space, so we actually subtract scores.
   * @param other
   * @return
   */
  def /(other: UnrefinedGrammarAnchoring[L, W]) = {
    // hacky multimethod dispatch is hacky
    if (other eq null) this // ugh
    else if (this eq other) new UnrefinedGrammarAnchoring.Identity[L, W](topology, lexicon, words, this.sparsityPattern)
    else if (other.isInstanceOf[UnrefinedGrammarAnchoring.Identity[L, W]]) this.addConstraints(other.sparsityPattern)
    else new ProductUnrefinedGrammarAnchoring(this, other, -1)
  }

  /** Is this CoreAnchoring nearly the same as that core anchoring? */
  def isConvergedTo(f: UnrefinedGrammarAnchoring[L, W], diff: Double) = isConvergedTo(f:GrammarAnchoring[L, W],diff)

  final def validLabelRefinements(begin: Int, end: Int, label: Int) = Array(0)

  final def numValidRefinements(label: Int) = 1

  final def numValidRuleRefinements(rule: Int) = 1

  private final val zeroArray = Array(0)
  final def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = zeroArray

  final def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = zeroArray

  final def leftChildRefinement(rule: Int, ruleRef: Int) = 0

  final def rightChildRefinement(rule: Int, ruleRef: Int) = 0

  final def parentRefinement(rule: Int, ruleRef: Int) = 0

  final def childRefinement(rule: Int, ruleRef: Int) = 0

  final def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = 0

  final def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = 0

  def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = topology.indexedBinaryRulesWithParent(a)

  def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = validLabelRefinements(begin, end, topology.parent(rule))

  def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, childRef: Int): Array[Int] = {
    zeroArray
  }

  def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] =  {
    zeroArray
  }

  def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int] =  {
    zeroArray
  }

  def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int] =  {
    zeroArray
  }

  /**
   * Scores the indexed [[epic.trees.UnaryRule]] rule when it occurs at (begin, end)
   */
  override def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
    scoreUnaryRule(begin, end, rule)
  }

  /**
   * Scores the indexed [[epic.trees.BinaryRule]] rule when it occurs at (begin, split, end)
   */
  override def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
    scoreBinaryRule(begin, split, end, rule)
  }

  /**
   * Scores the indexed label rule with refinenemnt ref, when it occurs at (begin, end). Can be used for s, or for a
   * "bottom" label. Mainly used for s.
   */
  override def scoreSpan(begin: Int, end: Int, label: Int, ref: Int): Double = {
    scoreSpan(begin, end, label)
  }
}

object UnrefinedGrammarAnchoring {
  /**
   * Returns an [[epic.parser.UnrefinedGrammarAnchoring.Identity]], which assigns 0
   * to everything that is allowed.
   * @param topology
   * @param lexicon
   * @param words
   * @tparam L
   * @tparam W
   * @return
   */
  def identity[L, W](topology: RuleTopology[L],
                     lexicon: Lexicon[L, W],
                     words: IndexedSeq[W],
                     constraints: ChartConstraints[L]):UnrefinedGrammarAnchoring[L, W] = {
    new Identity(topology, lexicon, words, constraints)
  }

  /**
   * Assigns 0 to everything
   * @param topology
   * @param lexicon
   * @param words
   * @tparam L
   * @tparam W
   * @return
   */
  @SerialVersionUID(1L)
  case class Identity[L, W](topology: RuleTopology[L], lexicon: Lexicon[L, W], words: IndexedSeq[W], sparsityPattern: ChartConstraints[L]) extends UnrefinedGrammarAnchoring[L, W] {
    //  def sparsityPattern = ChartConstraints.noSparsity[L]
    override def addConstraints(cs: ChartConstraints[L]): UnrefinedGrammarAnchoring[L, W] = copy(sparsityPattern = sparsityPattern & cs)
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0
    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0
    def scoreSpan(begin: Int, end: Int, tag: Int) = 0.0
  }

}
