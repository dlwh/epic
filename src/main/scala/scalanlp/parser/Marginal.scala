package scalanlp.parser

import scalanlp.trees.Rule
import scalanlp.util.TypeTags._

/**
 * Represents marginals over trees. Can also extract expected counts
 * @author dlwh
 */
trait Marginal[L, W] {
  def spec: WeightedGrammar[L, W]#Specialization
  def grammar = spec.grammar
  def partition: Double
  def words:Seq[W] = spec.words

  def expectedCounts[Feat](featurizer: SpanFeaturizer[L, W, Feat]) = {
    val spec = featurizer.specialize(words)
    val counts = new ExpectedCounts[Feat](featurizer.index)
    val visitor = Marginal.mkVisitor(counts, spec)
    visit(visitor)
    counts.loss = partition
    counts
  }

  /**
   * Visits the forest (if applicable) in whatever order is appropriate
   * @param spanVisitor
   */

  def visit(spanVisitor: AnchoredSpanVisitor[L]) { visitPostorder(spanVisitor) }
  /**
   * Forest traversal that visits spans in a "bottom up" order.
   * @param spanVisitor
   */
  def visitPostorder(spanVisitor: AnchoredSpanVisitor[L])
}

object Marginal {
  private def mkVisitor[L, W, Feat](counts: ExpectedCounts[Feat],
                                    spec: SpanFeaturizer[L, W, Feat]#Specialization):AnchoredSpanVisitor[L] = {
    new AnchoredSpanVisitor[L] {
      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
        addScale(counts, spec.featuresForBinaryRule(begin, split, end, rule, ref), score)
      }

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
        addScale(counts, spec.featuresForUnaryRule(begin, end, rule, ref), score)
      }

      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        addScale(counts, spec.featuresForSpan(begin, end, tag, ref), score)
      }
    }

  }

  private def addScale[Feat](counts: ExpectedCounts[Feat], features: Array[Int], score: Double) {
    for (f <- features) {
      counts.counts(f) += score
    }
  }
}
