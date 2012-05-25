package scalanlp.parser

import scalanlp.trees._

/**
 *
 * @author dlwh
 */

case class TreeMarginal[L, W](scorer: AugmentedAnchoring[L, W],
                              tree: BinarizedTree[(L,Int)]) extends Marginal[L, W] {

  val partition = {
    var score = 0.0
    def rec(t: BinarizedTree[(L,Int) ]):Unit = t match {
      case n@NullaryTree( (a, ref) ) =>
        val aI = grammar.labelIndex(a)
        score += scorer.scoreSpan(t.span.start, t.span.end, aI, ref)
        if(score.isInfinite) throw new Exception("Could not score gold tree!")
      case UnaryTree( (a, refA), child@Tree((b, refB), _)) =>
        val r = grammar.index(UnaryRule(a, b))
        val ruleRef = scorer.refined.ruleRefinementFromRefinements(r, refA, refB)

        score += scorer.scoreUnaryRule(t.span.start, t.span.end, r, ruleRef)
        if(score.isInfinite) throw new Exception("Could not score gold tree!")
        rec(child)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _), ct@Tree((c, refC), _)) =>
        val aI = grammar.labelIndex(a)
        val rule = grammar.index(BinaryRule(a, b, c))
        val ruleRef = scorer.refined.ruleRefinementFromRefinements(rule, refA, refB, refC)
        score += scorer.scoreSpan(t.span.start, t.span.end, aI, refA)
        score += scorer.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, rule, ruleRef)
        if(score.isInfinite) throw new Exception("Could not score gold tree!")
        rec(bt)
        rec(ct)
    }
    rec(tree)


    score
  }

  def visitPostorder(visitor: AnchoredVisitor[L]) {
    tree.postorder foreach {
      case n@NullaryTree( (a, ref) ) =>
        val aI = grammar.labelIndex(a)
        visitor.visitSpan(n.span.start, n.span.end, aI, ref, 1.0)
      case t@UnaryTree( (a, refA), Tree((b, refB), _)) =>
        val r = grammar.index(UnaryRule(a, b))
        val ruleRef = scorer.refined.ruleRefinementFromRefinements(r, refA, refB)
        visitor.visitUnaryRule(t.span.start, t.span.end, r, ruleRef, 1.0)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _), Tree((c, refC), _)) =>
        val aI = grammar.labelIndex(a)
        val rule = grammar.index(BinaryRule(a, b, c))
        val ruleRef = scorer.refined.ruleRefinementFromRefinements(rule, refA, refB, refC)
        visitor.visitSpan(t.span.start, t.span.end, aI, refA, 1.0)
        visitor.visitBinaryRule(t.span.start, bt.span.end, t.span.end, rule, ruleRef, 1.0)
    }
  }


}

object TreeMarginal {
  def apply[L, W](grammar: AugmentedGrammar[L, W],
                  words: Seq[W],
                  tree: BinarizedTree[(L,Int)]):TreeMarginal[L, W] = {
    TreeMarginal(grammar.specialize(words), tree)
  }
}