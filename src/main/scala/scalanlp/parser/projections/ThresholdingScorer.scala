package scalanlp.parser
package projections

/**
 * Takes another SpanScorer.Factory, and thresholds its outputs so that any thing > threshold is 0.0, and
 * anything else is Double.NegativeInfinity
 *
 * @author dlwh
 */
@SerialVersionUID(1)
class ThresholdingScorer[L](inner: SpanScorer[L], threshold: Double= -5.) extends SpanScorer[L] with Serializable {
  @inline private def I(score: Double) = if(score > threshold) score else Double.NegativeInfinity;

  def scoreSpan(begin: Int, end: Int, tag: Int) = I(inner.scoreSpan(begin,end,tag))

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = I(inner.scoreUnaryRule(begin,end,rule));

  def scoreBinaryRule(begin: Int, split: Int, end: Int, r: Int) = {
    I(inner.scoreBinaryRule(begin, split, end, r))
  }

}

