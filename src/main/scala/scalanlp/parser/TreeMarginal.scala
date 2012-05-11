package scalanlp.parser

import scalanlp.trees._

/**
 *
 * @author dlwh
 */

case class TreeMarginal[L, W](scorer: DerivationScorer[L, W],
                              tree: BinarizedTree[(L,Int)]) extends Marginal[L, W] {

  val partition = {
    var score = 0.0
    def rec(t: BinarizedTree[(L,Int) ]) = t match {
      case n@NullaryTree( (a, ref) ) =>
        val aI = grammar.labelIndex(a)
        var score2 = scorer.scoreSpan(t.span.start, t.span.end, aI, ref)
        if(score2.isInfinite)
          score2 = -20
        score += score2
      case UnaryTree( (a, refA), Tree((b, refB), _)) =>
        val r = grammar.index(UnaryRule(a, b))
        val ruleRef = scorer.ruleRefinementFromRefinements(r, refA, refB)
        var score2 = scorer.scoreUnaryRule(t.span.start, t.span.end, r, ruleRef)
        if(score2.isInfinite)
          score2 = -20
        score +=  score2
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _), Tree((c, refC), _)) =>
        val aI = grammar.labelIndex(a)
        val rule = grammar.index(BinaryRule(a, b, c))
        val ruleRef = scorer.ruleRefinementFromRefinements(rule, refA, refB, refC)
        var score2 = scorer.scoreSpan(t.span.start, t.span.end, aI, refA)
        if(score2.isInfinite)
          score2 = -20
        score += score2
        score += scorer.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, rule, ruleRef)
    }
    rec(tree)

    score
  }

  def visitPostorder(visitor: DerivationVisitor[L]) {
    tree.postorder foreach {
      case n@NullaryTree( (a, ref) ) =>
        val aI = grammar.labelIndex(a)
        visitor.visitSpan(n.span.start, n.span.end, aI, ref, 1.0)
      case t@UnaryTree( (a, refA), Tree((b, refB), _)) =>
        val r = grammar.index(UnaryRule(a, b))
        val ruleRef = scorer.ruleRefinementFromRefinements(r, refA, refB)
        visitor.visitUnaryRule(t.span.start, t.span.end, r, ruleRef, 1.0)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _), Tree((c, refC), _)) =>
        val aI = grammar.labelIndex(a)
        val rule = grammar.index(BinaryRule(a, b, c))
        val ruleRef = scorer.ruleRefinementFromRefinements(rule, refA, refB, refC)
        visitor.visitSpan(t.span.start, t.span.end, aI, refA, 1.0)
        visitor.visitBinaryRule(t.span.start, bt.span.end, t.span.end, rule, ruleRef, 1.0)
    }
  }


}

object TreeMarginal {
  def apply[L, W](grammar: DerivationScorer.Factory[L, W],
                  words: Seq[W],
                  tree: BinarizedTree[(L,Int)]):TreeMarginal[L, W] = {
    TreeMarginal(grammar.specialize(words), tree)
  }
}