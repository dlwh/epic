package scalanlp.parser

/**
 * SpanScorers are used in [[scalanlp.parser.ChartParser]]s to reweight rules in a particular context.
 * Typically, they're indexed for a *particular* set of rules and labels for speed.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait UnrefinedDerivationScorer[L, W] extends DerivationScorer[L, W] {
  // product and division (for EP)
  def *(scorer: UnrefinedDerivationScorer[L, W]) = UnrefinedDerivationScorer.product(this,scorer)
  def /(scorer: UnrefinedDerivationScorer[L, W]) = UnrefinedDerivationScorer.product(this,scorer,-1)
  
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

  final def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = Array(0)

  final def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = Array(0)

  final def leftChildRefinement(rule: Int, ruleRef: Int) = 0

  final def rightChildRefinement(rule: Int, ruleRef: Int) = 0

  final def parentRefinement(rule: Int, ruleRef: Int) = 0

  final def childRefinement(rule: Int, ruleRef: Int) = 0

  final def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = 0

  final def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = 0
}

object UnrefinedDerivationScorer {
  def identity[L, W]:UnrefinedDerivationScorer[L, W] = new UnrefinedDerivationScorer[L, W] {
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

    def scoreSpan(begin: Int, end: Int, tag: Int) = 0.0
  }

  def product[L, W](s1: UnrefinedDerivationScorer[L, W],
                    s2: UnrefinedDerivationScorer[L, W],
                    alpha: Double = 1.0):UnrefinedDerivationScorer[L,W] = {
    new UnrefinedDerivationScorer[L, W] {
      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double = {
        val r1 = s1.scoreBinaryRule(begin, split, end, rule);
        if(r1 == Double.NegativeInfinity) r1
        else r1 + alpha * s2.scoreBinaryRule(begin, split, end, rule);
      }
      def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double = {
        val r1 = s1.scoreUnaryRule(begin, end, rule);
        if(r1 == Double.NegativeInfinity) r1
        else r1 + alpha * s2.scoreUnaryRule(begin, end, rule);
      }

      def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
        val r1 = s1.scoreSpan(begin, end, tag);
        if(r1 == Double.NegativeInfinity) r1
        else r1 + alpha * s2.scoreSpan(begin, end, tag)
      }

    }

  }


}
