package scalanlp.parser

import scalanlp.inference.Factor

/**
 * [[scalanlp.parser.CoreAnchoring]] score rules and labels in a particular context
 * without needed extra "refined" categories. That is, an anchoring can
 * score x-bar spans in a particular context.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait CoreAnchoring[L, W] extends Factor[CoreAnchoring[L, W]] {
  def grammar: BaseGrammar[L]
  def lexicon: Lexicon[L, W]
  def words: Seq[W]

  /**
   * Scores the indexed [[scalanlp.trees.BinaryRule]] rule when it occurs at (begin,split,end)
   */
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double

  /**
   * Scores the indexed [[scalanlp.trees.UnaryRule]] rule when it occurs at (begin,end)
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
  def *(other: CoreAnchoring[L, W]) = {
    // hacky multimethod dispatch is hacky
    if (other eq null) this // ugh
    else if(other.isInstanceOf[CoreAnchoring.Identity[L, W]]) this
    else if(this.isInstanceOf[CoreAnchoring.Identity[L, W]]) other
    else new ProductCoreAnchoring(this,other)
  }

  /**
   * Computes the point-wise division of this grammar with some other grammar.
   *
   * Note that scores are in log space, so we actually subtract scores.
   * @param other
   * @return
   */
  def /(other: CoreAnchoring[L, W]) = {
    // hacky multimethod dispatch is hacky
    if (other eq null) this // ugh
    else if(other.isInstanceOf[CoreAnchoring.Identity[L, W]]) this
    else new ProductCoreAnchoring(this, other, -1)
  }

  /**
   * Computes the log-partition for this anchoring,
   * which is to say the inside score at the root.
   *
   * @return
   */
  def logPartition = marginal.partition

  /** Is this CoreAnchoring nearly the same as that core anchoring? */
  def isConvergedTo(f: CoreAnchoring[L, W], diff: Double) = lift.isConvergedTo(f.lift,diff)

  /** The posterior parse forest for this anchoring */
  def marginal = AugmentedAnchoring.fromCore(this).marginal

  def lift:RefinedAnchoring[L, W] = LiftedCoreAnchoring(this)
}

object CoreAnchoring {
  /**
   * Returns an [[scalanlp.parser.CoreAnchoring.Identity]], which assigns 0
   * to everything.
   * @param grammar
   * @param lexicon
   * @param words
   * @tparam L
   * @tparam W
   * @return
   */
  def identity[L, W](grammar: BaseGrammar[L],
                     lexicon: Lexicon[L, W],
                     words: Seq[W]):CoreAnchoring[L, W] = {
    new Identity(grammar, lexicon, words)
  }

  /**
   * Assigns 0 to everything
   * @param grammar
   * @param lexicon
   * @param words
   * @tparam L
   * @tparam W
   * @return
   */
  @SerialVersionUID(1L)
  case class Identity[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W], words: Seq[W]) extends CoreAnchoring[L, W] {

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

    def scoreSpan(begin: Int, end: Int, tag: Int) = 0.0
  }

}

/**
 * Turns a [[scalanlp.parser.CoreAnchoring]] into a [[scalanlp.parser.RefinedAnchoring]]
 * @param core
 * @tparam L
 * @tparam W
 */
@SerialVersionUID(1)
case class LiftedCoreAnchoring[L, W](core: CoreAnchoring[L, W]) extends RefinedAnchoring[L, W] {
  override def annotationTag = 0

  def grammar = core.grammar

  def lexicon = core.lexicon

  def words = core.words

  final def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
    core.scoreSpan(begin, end, label)
  }

  final def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
    core.scoreBinaryRule(begin, split, end, rule)
  }

  final def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
    core.scoreUnaryRule(begin, end, rule)
  }

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

  def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = grammar.indexedBinaryRulesWithParent(a)
}



