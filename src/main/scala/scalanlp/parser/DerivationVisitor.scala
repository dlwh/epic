package scalanlp.parser

/**
 * A class that asks about all anchored spans.
 * It's the "foreach" version of a spanscorer, that takes in
 * expected counts.
 * @author dlwh
 */
trait DerivationVisitor[L] {
  def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double)
  def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double)
  def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double)
}

object DerivationVisitor {
  def noOp[L]:DerivationVisitor[L] = new DerivationVisitor[L] {
    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) = {}

    def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {}

    def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {}
  }
}