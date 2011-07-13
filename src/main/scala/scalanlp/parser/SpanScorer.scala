package scalanlp.parser

/**
 * SpanScorers are used in ChartParsers to reweight rules in a particular context.
 * Typically, they're indexed for a *particular* grammar for speed.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait SpanScorer[T] extends Serializable {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double
  def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double
  def scoreSpan(begin: Int, end: Int, tag: Int): Double;
}

object SpanScorer {
  def sum[T](s1: SpanScorer[T], s2: SpanScorer[T]):SpanScorer[T] = new SpanScorer[T] {
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

  def divide[T](s: SpanScorer[T], div: Double):SpanScorer[T] = new SpanScorer[T] {
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double = {
      s.scoreBinaryRule(begin, split, end, rule) / div;
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double = {
      s.scoreUnaryRule(begin, end, rule) / div;
    }

    def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
      s.scoreSpan(begin, end, tag) / div;
    }

  }

  @SerialVersionUID(1)
  trait Factory[C,F,-W] extends Serializable {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[F] = identity):SpanScorer[C]
  }

  @SerialVersionUID(1)
  def identityFactory[C,F,W]:Factory[C,F,W] = new Factory[C,F,W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[F] = identity) = identity[C];
  }


  @SerialVersionUID(1)
  def forwardingFactory[C,W]:Factory[C,C,W] = new Factory[C,C,W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[C] = identity) = oldScorer;
  }

  def identity[T]:SpanScorer[T] = new SpanScorer[T] {
    def scoreSpan(begin: Int, end: Int, tag: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0
  }



}