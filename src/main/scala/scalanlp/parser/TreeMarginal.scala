package scalanlp.parser

import scalanlp.trees._

/**
 * This isn't really a marginal, except in a degenerate sense.
 * It gives the likelihood of a known fixed tree under the
 * Anchoring and give expected counts (i.e. count the
 * occurrences of each rule.)
 *
 * @param anchoring The grammar anchoring
 * @param tree A tree that has been decorated with
 *             the gold refinements at each leaf
 * @author dlwh
 */
case class TreeMarginal[L, W](anchoring: AugmentedAnchoring[L, W],
                              tree: BinarizedTree[(L,Int)]) extends Marginal[L, W] {

  val partition = {
    var score = 0.0
    def rec(t: BinarizedTree[(L,Int) ]):Unit = t match {
      case n@NullaryTree( (a, ref), span ) =>
        val aI = grammar.labelIndex(a)
        score += anchoring.scoreSpan(span.start, span.end, aI, ref)
        if(score.isInfinite) throw new Exception("Could not score gold tree!")
      case UnaryTree( (a, refA), child@Tree((b, refB), _, _), span) =>
        val r = grammar.index(UnaryRule(a, b))
        val ruleRef = anchoring.refined.ruleRefinementFromRefinements(r, refA, refB)
        if(ruleRef < 0) throw new Exception("Bad refined rule in gold tree!: " + UnaryRule(a, b) + " aRef: " + refA + " bRef: " + refB)

        score += anchoring.scoreUnaryRule(t.span.start, t.span.end, r, ruleRef)
        if(score.isInfinite) throw new Exception("Could not score gold tree!")
        rec(child)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _, _), ct@Tree((c, refC), _, _), span) =>
        val aI = grammar.labelIndex(a)
        val rule = grammar.index(BinaryRule(a, b, c))
        val ruleRef = anchoring.refined.ruleRefinementFromRefinements(rule, refA, refB, refC)
        score += anchoring.scoreSpan(t.span.start, t.span.end, aI, refA)
        score += anchoring.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, rule, ruleRef)
        if(score.isInfinite) throw new Exception("Could not score gold tree!")
        rec(bt)
        rec(ct)
    }
    rec(tree)


    score
  }

  def visitPostorder(visitor: AnchoredVisitor[L]) {
    tree.postorder foreach {
      case n@NullaryTree( (a, ref), span ) =>
        val aI = grammar.labelIndex(a)
        visitor.visitSpan(n.span.start, n.span.end, aI, ref, 1.0)
      case t@UnaryTree( (a, refA), Tree((b, refB), _, _), span) =>
        val r = grammar.index(UnaryRule(a, b))
        val ruleRef = anchoring.refined.ruleRefinementFromRefinements(r, refA, refB)
        if(ruleRef < 0) throw new Exception("Bad refined rule in gold tree!: " + UnaryRule(a, b) + " aRef: " + refA + " bRef: " + refB)
        visitor.visitUnaryRule(t.span.start, t.span.end, r, ruleRef, 1.0)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _, _), Tree((c, refC), _, _), span) =>
        val aI = grammar.labelIndex(a)
        val rule = grammar.index(BinaryRule(a, b, c))
        val ruleRef = anchoring.refined.ruleRefinementFromRefinements(rule, refA, refB, refC)
        visitor.visitSpan(t.span.start, t.span.end, aI, refA, 1.0)
        visitor.visitBinaryRule(t.span.start, bt.span.end, t.span.end, rule, ruleRef, 1.0)
    }
  }


}

object TreeMarginal {
  def apply[L, W](grammar: AugmentedGrammar[L, W],
                  words: Seq[W],
                  tree: BinarizedTree[(L,Int)]):TreeMarginal[L, W] = {
    TreeMarginal(grammar.anchor(words), tree)
  }
}