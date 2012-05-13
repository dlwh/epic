package scalanlp.parser

/**
 * SpanScorers are used in [[scalanlp.parser.ChartParser]]s to reweight rules in a particular context.
 * Typically, they're indexed for a *particular* set of rules and labels for speed.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait UnrefinedDerivationScorer[L, W] extends DerivationScorer[L, W] {
  override final def annotationTag = 0

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

  final def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
    scoreSpan(begin, end, label)
  }

  final def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
    scoreBinaryRule(begin, split, end, rule)
  }

  final def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
    scoreUnaryRule(begin, end, rule)
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

object UnrefinedDerivationScorer {
  def identity[L, W](grammar: Grammar[L],
                     lexicon: Lexicon[L, W],
                     words: Seq[W]):UnrefinedDerivationScorer[L, W] = {
    new Identity(grammar, lexicon, words)
  }

  @SerialVersionUID(1L)
  case class Identity[L, W](grammar: Grammar[L], lexicon: Lexicon[L, W], words: Seq[W]) extends UnrefinedDerivationScorer[L, W] {

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

    def scoreSpan(begin: Int, end: Int, tag: Int) = 0.0
  }
}

