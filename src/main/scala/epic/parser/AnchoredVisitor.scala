package epic.parser

/**
 * A class that asks about all anchored spans.
 * It's the "foreach" version of a [[epic.parser.RefinedAnchoring]] that takes in
 * expected counts.
 * @author dlwh
 */
trait AnchoredVisitor[L] {
  def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double)
  def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double)
  def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double)
}
