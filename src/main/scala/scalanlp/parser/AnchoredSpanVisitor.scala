package scalanlp.parser

import scalanlp.util.Index
import scalanlp.trees._

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

  def visitBinarizedTree[L](labelIndex: Index[L],
                            ruleIndex: Index[Rule[L]],
                            tree: BinarizedTree[L],
                            v: AnchoredSpanVisitor,
                            spanScorer: SpanScorer[L] = SpanScorer.identity[L]):Double = {
    var score = 0.0
    for(t2 <- tree.allChildren) {
      t2 match {
        case BinaryTree(a,bt@ Tree(b,_),Tree(c,_)) =>
          val r = ruleIndex(BinaryRule(a,b,c))
          val aI = labelIndex(a)
          v.visitBinaryRule(t2.span.start, bt.span.end, t2.span.end, r, 1.0)
          v.visitSpan(t2.span.start, t2.span.end, aI, 1.0)
          val sScore = spanScorer.scoreSpan(t2.span.start,t2.span.end,aI)
          if(!sScore.isInfinite) score += sScore
          else println(":(")
          score += spanScorer.scoreBinaryRule(t2.span.start, bt.span.end, t2.span.end, r)
        case UnaryTree(a,Tree(b,_)) =>
          val r = ruleIndex(UnaryRule(a,b))
          v.visitUnaryRule(t2.span.start, t2.span.end, r, 1.0)
          score += spanScorer.scoreUnaryRule(t2.span.start, t2.span.end, r)
        case n@NullaryTree(a) =>
          val aI = labelIndex(a)
          v.visitSpan(t2.span.start, t2.span.end, aI, 1.0)
          val sScore = spanScorer.scoreSpan(t2.span.start,t2.span.end,aI)
          if(!sScore.isInfinite) score += sScore
          else println(":(")
      }
    }
    score
  }

}