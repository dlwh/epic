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
      val start = t2.span.start
      val end = t2.span.end
      t2 match {
        case BinaryTree(a,bt@ Tree(b,_),Tree(c,_)) =>
          val r = ruleIndex(BinaryRule(a,b,c))
          val aI = labelIndex(a)
          v.visitBinaryRule(start, bt.span.end, end, r, 1.0)
          v.visitSpan(start, end, aI, 1.0)
          val sScore = spanScorer.scoreSpan(start,end,aI)
          if(!sScore.isInfinite) score += sScore
          else println(":(")
          score += spanScorer.scoreBinaryRule(start, bt.span.end, end, r)
        case UnaryTree(a,Tree(b,_)) =>
          val r = ruleIndex(UnaryRule(a,b))
          v.visitUnaryRule(start, end, r, 1.0)
          val sScore = spanScorer.scoreUnaryRule(start, end, r)
          if(!sScore.isInfinite) score += sScore
          else println(":(")
        case n@NullaryTree(a) =>
          val aI = labelIndex(a)
          v.visitSpan(start, end, aI, 1.0)
          val sScore = spanScorer.scoreSpan(start,end,aI)
          if(!sScore.isInfinite) score += sScore
          else println(":(")
      }
    }
    score
  }

}