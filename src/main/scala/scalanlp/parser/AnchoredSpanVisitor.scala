package scalanlp.parser

import scalanlp.trees._
import scalanlp.util.TypeTags.ID

/**
 * A class that asks about all anchored spans.
 * It's the "foreach" version of a spanscorer, that takes in
 * expected counts.
 * @author dlwh
 */
trait AnchoredSpanVisitor[L] {
  def visitBinaryRule(begin: Int, split: Int, end: Int, rule: ID[Rule[L]], ref: ID[RuleRef[L]], score: Double)
  def visitUnaryRule(begin: Int, end: Int, rule: ID[Rule[L]], ref: ID[RuleRef[L]], score: Double)
  def visitSpan(begin: Int, end: Int, tag: ID[L], ref: ID[Ref[L]], score: Double)
}

object AnchoredSpanVisitor {
  def noOp[L]:AnchoredSpanVisitor[L] = new AnchoredSpanVisitor[L] {
    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: ID[Rule[L]], ref: ID[RuleRef[L]], score: Double) = {}

    def visitUnaryRule(begin: Int, end: Int, rule: ID[Rule[L]], ref: ID[RuleRef[L]], score: Double) {}

    def visitSpan(begin: Int, end: Int, tag: ID[L], ref: ID[Ref[L]], score: Double) {}
  }
}