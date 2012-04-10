package scalanlp.parser

/**
 * SpanScorers are used in [[scalanlp.parser.ChartParser]]s to reweight rules in a particular context.
 * Typically, they're indexed for a *particular* set of rules and labels for speed.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait SpanScorer[L] extends Serializable {
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
  def scoreSpan(begin: Int, end: Int, tag: Int): Double;

}

object SpanScorer {
  /**
   * Returns the sum of two span scores, by adding them together.
   */
  def sum[L](s1: SpanScorer[L], s2: SpanScorer[L]):SpanScorer[L] = new SpanScorer[L] {
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double = {
      val r1 = s1.scoreBinaryRule(begin, split, end, rule);
      if(r1 == Double.NegativeInfinity) r1
      else r1 + s2.scoreBinaryRule(begin, split, end, rule);
    }
    def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double = {
      val r1 = s1.scoreUnaryRule(begin, end, rule);
      if(r1 == Double.NegativeInfinity) r1
      else r1 + s2.scoreUnaryRule(begin, end, rule);
    }

    def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
      val r1 = s1.scoreSpan(begin, end, tag);
      if(r1 == Double.NegativeInfinity) r1
      else r1 + s2.scoreSpan(begin, end, tag)
    }
  }

  /**
   * A SpanScorer.Factory can build a SpanScorer from a sentence. Can be
   * used to compute features specific to a sentence, or to preparse the
   * sentence and return a scorer based on that.
   */
  @SerialVersionUID(1)
  trait Factory[L,-W] extends Serializable {
    def mkSpanScorer(s: Seq[W], goldLabel: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):SpanScorer[L]
  }

  /**
   * The identity scorer returns 0 always.
   */
  def identity[L]:SpanScorer[L] = new SpanScorer[L] {
    def scoreSpan(begin: Int, end: Int, tag: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0
  }
}

