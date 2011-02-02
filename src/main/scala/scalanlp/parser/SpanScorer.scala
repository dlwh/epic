package scalanlp.parser

/**
 * SpanScorers are used in ChartParsers to reweight rules in a particular context.
 * Typically, they're indexed for a *particular* grammar for speed.
 *
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
trait SpanScorer[T] {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int): Double
  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int): Double
  def scoreLexical(begin: Int, end: Int, tag: Int): Double;
}

object SpanScorer {
  def sum[T](s1: SpanScorer[T], s2: SpanScorer[T]):SpanScorer[T] = new SpanScorer[T] {
    def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int): Double = {
      val r1 = s1.scoreBinaryRule(begin, split, end, parent, leftChild, rightChild);
      if(r1 == Double.NegativeInfinity) r1
      else r1 + s2.scoreBinaryRule(begin, split, end, parent, leftChild, rightChild);
    }
    def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int): Double = {
      val r1 = s1.scoreUnaryRule(begin, end, parent, child);
      if(r1 == Double.NegativeInfinity) r1
      else r1 + s2.scoreUnaryRule(begin, end, parent, child);
    }

    def scoreLexical(begin: Int, end: Int, tag: Int): Double = {
      val r1 = s1.scoreLexical(begin, end, tag);
      if(r1 == Double.NegativeInfinity) r1
      else r1 + s2.scoreLexical(begin, end, tag)
    }
  }

  def divide[T](s: SpanScorer[T], div: Double):SpanScorer[T] = new SpanScorer[T] {
    def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int): Double = {
      s.scoreBinaryRule(begin, split, end, parent, leftChild, rightChild) / div;
    }

    def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int): Double = {
      s.scoreUnaryRule(begin, end, parent, child) / div;
    }

    def scoreLexical(begin: Int, end: Int, tag: Int): Double = {
      s.scoreLexical(begin, end, tag) / div;
    }

  }

  @serializable
  trait Factory[C,F,-W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[F] = identity):SpanScorer[C]
  }

  @serializable
  @SerialVersionUID(1)
  def identityFactory[C,F,W]:Factory[C,F,W] = new Factory[C,F,W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[F] = identity) = identity[C];
  }


  @serializable
  @SerialVersionUID(1)
  def forwardingFactory[C,W]:Factory[C,C,W] = new Factory[C,C,W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[C] = identity) = oldScorer;
  }

  def identity[T]:SpanScorer[T] = new SpanScorer[T] {
    def scoreLexical(begin: Int, end: Int, tag: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = 0.0

    def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = 0.0
  }



}