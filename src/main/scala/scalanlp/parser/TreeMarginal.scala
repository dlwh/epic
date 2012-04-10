package scalanlp.parser

import scalanlp.util.TypeTags._
import scalanlp.trees._


/**
 *
 * @author dlwh
 */

case class TreeMarginal[L, W](grammar: Grammar[L],
                              spec: DerivationScorer[L, W],
                              tree: BinarizedTree[(L,Int)]) extends Marginal[L, W] {

  val partition = {
    var score = 0.0
    def rec(t: BinarizedTree[(L,Int) ]) = t match {
      case n@NullaryTree( (a, ref) ) =>
        val aI = tag[L](grammar.labelIndex(a))
        score += spec.scoreSpan(t.span.start, t.span.end, aI, ref)
      case UnaryTree( (a, refA), Tree((b, refB), _)) =>
        val r = tag[Rule[L]](grammar.index(UnaryRule(a, b)))
        val ruleRef = spec.ruleRefinementFromRefinements(r, refA, refB)
        score +=  spec.scoreUnaryRule(t.span.start, t.span.end, tag(r), ruleRef)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _), Tree((c, refC), _)) =>
        val aI = tag[L](grammar.labelIndex(a))
        val rule = tag[Rule[L]](grammar.index(BinaryRule(a, b, c)))
        val ruleRef = spec.ruleRefinementFromRefinements(rule, refA, refB, refC)
        score += spec.scoreSpan(t.span.start, t.span.end, aI, refA)
        score += spec.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, tag(rule), ruleRef)
    }
    rec(tree)

    score
  }

  def visitPostorder(visitor: DerivationVisitor[L]) {
    tree.postorder foreach {
      case n@NullaryTree( (a, ref) ) =>
        val aI = tag[L](grammar.labelIndex(a))
        visitor.visitSpan(n.span.start, n.span.end, aI, ref, 1.0)
      case t@UnaryTree( (a, refA), Tree((b, refB), _)) =>
        val r = tag[Rule[L]](grammar.index(UnaryRule(a, b)))
        val ruleRef = spec.ruleRefinementFromRefinements(r, refA, refB)
        visitor.visitUnaryRule(t.span.start, t.span.end, r, ruleRef, 1.0)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _), Tree((c, refC), _)) =>
        val aI = tag[L](grammar.labelIndex(a))
        val rule = tag[Rule[L]](grammar.index(BinaryRule(a, b, c)))
        val ruleRef = spec.ruleRefinementFromRefinements(rule, refA, refB, refC)
        visitor.visitSpan(t.span.start, t.span.end, aI, refA, 1.0)
        visitor.visitBinaryRule(t.span.start, bt.span.end, t.span.end, tag(rule), ruleRef, 1.0)
    }
  }


}

object TreeMarginal {
  def apply[L, W](grammar: DerivationScorer.Factory[L, W],
                  words: Seq[W],
                  tree: BinarizedTree[(L,Int)]):TreeMarginal[L, W] = {
    TreeMarginal(grammar.grammar, grammar.specialize(words), tree)
  }
}