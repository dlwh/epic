package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
object ScalingSpanScorer {
  def apply[C](num: SpanScorer[C], denom: SpanScorer[C], constant:Double, root: Int):SpanScorer[C] = new SpanScorer[C] {
    def scoreSpan(begin: Int, end: Int, tag: Int) = {
      val ns = num.scoreSpan(begin, end, tag)
      if(ns == Double.NegativeInfinity) ns
      else {
        val ds = denom.scoreSpan(begin, end, tag)
        if(ns == Double.NegativeInfinity || ds == Double.NegativeInfinity) Double.NegativeInfinity;
        else ns - ds + {if(tag == root) constant else 0.0 }
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
      if(ns == Double.NegativeInfinity) ns
      else {
        val ds = denom.scoreBinaryRule(begin, split, end, rule)
        if(ns == Double.NegativeInfinity || ds == Double.NegativeInfinity) Double.NegativeInfinity;
        else ns - ds
      }
    }
  }

  /*
  def rescaleParentScorer(num: SpanScorer, denom: SpanScorer, constant: Double, len: Int, numSymbols: Int) = {
    val scores = TriangularArray.raw(inside.length+1,null:SparseVector);
    var density = 0;
    var labelDensity = 0;
    for(begin <-  0 until inside.length; end <- begin+1 to (inside.length)) {
      val index = TriangularArray.index(begin, end)
      for(l <- 0 until numSymbols) {
        val pL = indexedProjections.project(l)
        val myScore =
        if(scores(index) == null) {
          scores(index) = new SparseVector(numSymbols,1);
          density += 1;
        }
        val currentScore = scores(index)(pL);
        if(currentScore == Double.NegativeInfinity) labelDensity += 1;
        scores(index)(pL) = Numerics.logSum(currentScore,myScore);
      }
    }
    new LabeledSpanScorer(scores);
  }
  */
}