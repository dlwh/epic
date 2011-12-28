package scalanlp.parser
package projections



import java.io._;

/**
 * Takes another SpanScorer.Factory, and thresholds its outputs so that any thing > threshold is 0.0, and
 * anything else is Double.NegativeInfinity
 *
 * Ignores thresholdScorer
 *
 * @author dlwh
 */
class StepFunctionSpanScorerFactory[L,W](innerFactory: SpanScorer.Factory[L,L,W], threshold: Double= -7) extends SpanScorer.Factory[L,L,W] {
  def mkSpanScorer(s: scala.Seq[W], oldScorer: SpanScorer[L], thresholdScorer: GoldTagPolicy[L]):SpanScorer[L] = {
    val inner = innerFactory.mkSpanScorer(s,oldScorer);
    new StepFunctionSpanScorer(inner, threshold);
  }

}

@SerialVersionUID(1)
final class StepFunctionSpanScorer[L](inner: SpanScorer[L], threshold: Double = -7) extends SpanScorer[L] with Serializable {
  @inline def I(score: Double) = if(score > threshold) 0.0 else Double.NegativeInfinity;

  def scoreSpan(begin: Int, end: Int, tag: Int) = I(inner.scoreSpan(begin,end,tag))

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = I(inner.scoreUnaryRule(begin,end,rule));

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    I(inner.scoreBinaryRule(begin, split, end, rule))
  }
}

