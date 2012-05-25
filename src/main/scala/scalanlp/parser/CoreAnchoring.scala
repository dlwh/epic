package scalanlp.parser

import scalanlp.inference.Factor

/**
 * SpanScorers are used in [[scalanlp.parser.ChartParser]]s to reweight rules in a particular context.
 * Typically, they're indexed for a *particular* set of rules and labels for speed.
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
  def *(other: CoreAnchoring[L, W]) = {
    // hacky multimethod dispatch is hacky
    if (other eq null) this // ugh
    else if(other.isInstanceOf[CoreAnchoring.Identity[L, W]]) this
    else if(this.isInstanceOf[CoreAnchoring.Identity[L, W]]) other
    else new ProductCoreAnchoring(this,other)
  }

    // Factor stuff
  def /(other: CoreAnchoring[L, W]) = {
    // hacky multimethod dispatch is hacky
    if (other eq null) this // ugh
    else if(other.isInstanceOf[CoreAnchoring.Identity[L, W]]) this
    else new ProductCoreAnchoring(this, other, -1)
  }

  def logPartition = marginal.partition

  def isConvergedTo(f: CoreAnchoring[L, W], diff: Double) = lift.isConvergedTo(f.lift,diff)

  lazy val marginal = AugmentedAnchoring.fromCore(this).marginal

  def lift:RefinedAnchoring[L, W] = LiftedCoreAnchoring(this)
}

object CoreAnchoring {
  def identity[L, W](grammar: BaseGrammar[L],
                     lexicon: Lexicon[L, W],
                     words: Seq[W]):CoreAnchoring[L, W] = {
    new Identity(grammar, lexicon, words)
  }

  @SerialVersionUID(1L)
  case class Identity[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W], words: Seq[W]) extends CoreAnchoring[L, W] {

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

    def scoreSpan(begin: Int, end: Int, tag: Int) = 0.0
  }

}

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

}



