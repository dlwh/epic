package scalanlp.parser

import scalanlp.trees.BinarizedTree
import scalanlp.collection.mutable.TriangularArray

/**
 * SpanScorers are used in [[scalanlp.parser.ChartParser]]s to reweight rules in a particular context.
 * Typically, they're indexed for a *particular* set of rules and labels for speed.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait SpanScorer[T] extends Serializable {
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

  /**
   * Divides a SpanScorer by some scalar
   */
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

  /**
   * A SpanScorer.Factory can build a SpanScorer from a sentence. Can be
   * used to compute features specific to a sentence, or to preparse the
   * sentence and return a scorer based on that.
   */
  @SerialVersionUID(1)
  trait Factory[C,F,-W] extends Serializable {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[C] = identity,
                     goldLabel: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags[C]):SpanScorer[C]
  }

  /**
   * Always returns the identity, completely ignoring oldScorer
   */
  @SerialVersionUID(1)
  def identityFactory[C,F,W]:Factory[C,F,W] = new Factory[C,F,W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[C], goldTag: GoldTagPolicy[C]) = identity[C];
  }


  /**
   * Always returns oldScorer
   */
  @SerialVersionUID(1)
  def forwardingFactory[C,W]:Factory[C,C,W] = new Factory[C,C,W] {
    def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[C], goldTag: GoldTagPolicy[C]) = oldScorer;
  }

  /**
   * The identity scorer returns 0 always.
   */
  def identity[T]:SpanScorer[T] = new SpanScorer[T] {
    def scoreSpan(begin: Int, end: Int, tag: Int) = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0
  }

  /**
   * Constant scorer returns the same thing for all applications
   */
  def constant[L](labelThreshold: Double, ruleThreshold: Double = Double.NegativeInfinity):SpanScorer[L] = new SpanScorer[L] {
    def scoreSpan(begin: Int, end: Int, tag: Int) = labelThreshold

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = ruleThreshold

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = ruleThreshold
  }


}

trait GoldTagPolicy[L] {
  def isGoldTag(start: Int, end: Int, tag: Int):Boolean
}

object GoldTagPolicy {
  def noGoldTags[L]:GoldTagPolicy[L] = new GoldTagPolicy[L] {
    def isGoldTag(start: Int, end: Int, tag: Int) = false
  }

  def candidateTreeForcing[L](tree: BinarizedTree[Seq[Int]]):GoldTagPolicy[L] ={
    val gold = TriangularArray.raw(tree.span.end+1,collection.mutable.BitSet());
    if(tree != null) {
      for( t <- tree.allChildren) {
        gold(TriangularArray.index(t.span.start,t.span.end)) ++= t.label
      }
    }
    new GoldTagPolicy[L] {
      def isGoldTag(start: Int, end: Int, tag: Int) = {
        val set = gold(TriangularArray.index(start,end))
        set != null && set.contains(tag)
      }
    }
  }


  def goldTreeForcing[L](tree: BinarizedTree[Int]):GoldTagPolicy[L] ={
    val gold = TriangularArray.raw(tree.span.end+1,-1)
    if(tree != null) {
      for( t <- tree.allChildren) {
        gold(TriangularArray.index(t.span.start,t.span.end)) = t.label
      }
    }
    new GoldTagPolicy[L] {
      def isGoldTag(start: Int, end: Int, tag: Int) = {
        gold(TriangularArray.index(start,end)) == tag
      }
    }
  }
}