package scalanlp.parser

/**
 * Represents marginals over trees. Can also extract expected counts
 * @author dlwh
 */
trait Marginal[L, W] {
  def scorer: DerivationScorer[L, W]
  def grammar:Grammar[L] = scorer.grammar
  def lexicon = scorer.lexicon
  def partition: Double
  def words:Seq[W] = scorer.words
  def length = words.length

  def expectedCounts[Feat](featurizer: DerivationFeaturizer[L, W, Feat]): ExpectedCounts[Feat] = {
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

  def visit(spanVisitor: DerivationVisitor[L]) { visitPostorder(spanVisitor) }
  /**
   * Forest traversal that visits spans in a "bottom up" order.
   * @param spanVisitor
   */
  def visitPostorder(spanVisitor: DerivationVisitor[L])
}

object Marginal {
  private def mkVisitor[L, W, Feat](counts: ExpectedCounts[Feat],
                                    spec: DerivationFeaturizer[L, W, Feat]#Specialization):DerivationVisitor[L] = {
    new DerivationVisitor[L] {
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
