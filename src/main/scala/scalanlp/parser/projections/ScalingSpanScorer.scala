package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
object ScalingSpanScorer {
  def apply[C, W](num: UnrefinedDerivationScorer[C, W], denom: UnrefinedDerivationScorer[C, W]):UnrefinedDerivationScorer[C, W] = new UnrefinedDerivationScorer[C, W] {
    def scoreSpan(begin: Int, end: Int, tag: Int) = {
      val ns = num.scoreSpan(begin, end, tag)
      if(ns == Double.NegativeInfinity) ns
      else {
        val ds = denom.scoreSpan(begin, end, tag)
        if(ns == Double.NegativeInfinity || ds == Double.NegativeInfinity) Double.NegativeInfinity;
        else ns - ds
      }
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
      val ns = num.scoreUnaryRule(begin, end, rule)
      if(ns == Double.NegativeInfinity) ns
      else {
        val ds = denom.scoreUnaryRule(begin, end, rule)
        if(ns == Double.NegativeInfinity || ds == Double.NegativeInfinity) Double.NegativeInfinity;
        else ns - ds
      }
    }

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
      val ns = num.scoreBinaryRule(begin, split, end, rule)
      if (ns == Double.NegativeInfinity) ns
      else {
        val ds = denom.scoreBinaryRule(begin, split, end, rule)
        if (ds == Double.NegativeInfinity) Double.NegativeInfinity;
        else ns - ds
      }
    }
  }

}