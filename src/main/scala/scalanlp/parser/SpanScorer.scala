package scalanlp.parser

/**
 * SpanWeighters are used in ChartParsers to reweight labeled spans and rules.
 * Typically, they're indexed for a *particular* grammar for speed.
 *
 *
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
trait SpanScorer {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int): Double
  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int): Double
  def scoreLexical(begin: Int, end: Int, tag: Int): Double;
}

object SpanScorer {
  def sum(s1: SpanScorer, s2: SpanScorer):SpanScorer = new SpanScorer {
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

  def project(fineToCoarseMap: Int=>Int)(coarseScorer: SpanScorer): SpanScorer = new SpanScorer {
    def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int): Double = {
      coarseScorer.scoreBinaryRule(begin, split, end, fineToCoarseMap(parent), fineToCoarseMap(leftChild), fineToCoarseMap(rightChild))
    }
    def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int): Double = {
      coarseScorer.scoreUnaryRule(begin, end, fineToCoarseMap(parent), fineToCoarseMap(child))
    }

    def scoreLexical(begin: Int, end: Int, tag: Int): Double = {
      coarseScorer.scoreLexical(begin, end, fineToCoarseMap(tag))
    }
  }

  trait Factory[W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer = identity):SpanScorer
  }

  def identityFactory[W]:Factory[W] = new Factory[W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer = identity) = oldScorer;
  }

  val identity = new SpanScorer {
    def scoreLexical(begin: Int, end: Int, tag: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = 0.0

    def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = 0.0
  }

}