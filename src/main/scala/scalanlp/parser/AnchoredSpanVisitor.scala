package scalanlp.parser

/**
 * A class that asks about all anchored spans.
 * It's the "foreach" version of a spanscorer, that takes in
 * expected counts.
 * @author dlwh
 */
trait AnchoredSpanVisitor {
  def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, score: Double)
  def visitUnaryRule(begin: Int, end: Int, rule: Int, score: Double)
  def visitSpan(begin: Int, end: Int, tag: Int, score: Double)
}

object AnchoredSpanVisitor {
  val noOp:AnchoredSpanVisitor = new AnchoredSpanVisitor {
    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, score: Double) = ()

    def visitUnaryRule(begin: Int, end: Int, rule: Int, score: Double) = ()

    def visitSpan(begin: Int, end: Int, tag: Int, score: Double) = ()
  }

}