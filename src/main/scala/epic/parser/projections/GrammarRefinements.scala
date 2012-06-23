package epic.parser
package projections

import breeze.util.Index
import epic.trees._

/**
 * 
 * @author dlwh
 */

@SerialVersionUID(1L)
case class GrammarRefinements[C, F](labels: ProjectionIndexer[C, F], rules: ProjectionIndexer[Rule[C], Rule[F]]) {
  def compose[F2](other: GrammarRefinements[F, F2]):GrammarRefinements[C, F2] = new GrammarRefinements(labels compose other.labels, rules compose other.rules)

}

object GrammarRefinements {
  def identity[L](grammar: BaseGrammar[L]): GrammarRefinements[L, L] = {
    apply(grammar, grammar, Predef.identity[L] _)
  }

  def apply[C, F](coarse: BaseGrammar[C], fine: BaseGrammar[F], proj: F=>C): GrammarRefinements[C, F] = {
    def projRule(r: Rule[F]) = r map proj
    val rules = ProjectionIndexer(coarse.index, fine.index, projRule)
    val labels = ProjectionIndexer(coarse.labelIndex, fine.labelIndex, proj)

    new GrammarRefinements(labels, rules)
  }

  def apply[C, F](coarse: BaseGrammar[C], split: C=>Seq[F], proj: F=>C): GrammarRefinements[C, F] = {
    def splitRule(r: Rule[C]) = r match {
      case BinaryRule(a, b, c) => for(a_ <- split(a); b_ <- split(b); c_ <- split(c)) yield BinaryRule(a_, b_, c_)
      case UnaryRule(a, b, chain) => for(a_ <- split(a); b_ <- split(b)) yield UnaryRule(a_, b_, chain)
    }
    apply(coarse, split, splitRule, proj)
  }

  def apply[C, F](coarse: BaseGrammar[C], split: C=>Seq[F], splitRule: Rule[C]=>Seq[Rule[F]], proj: F=>C): GrammarRefinements[C, F] = {
    val fineIndex = {
      val index = Index[F]()
      for( l <- coarse.labelIndex; l2 <- split(l)) {
        index.index(l2)
      }
      index
    }
    val ruleIndex = {
      val index = Index[Rule[F]]()
      for( r <- 0 until coarse.index.size; split <- splitRule(coarse.index.get(r))) {
        index.index(split)
      }
      index
    }

    def projRule(r: Rule[F]) = r map proj

    val rules = ProjectionIndexer(coarse.index, ruleIndex, projRule)
    val labels = ProjectionIndexer(coarse.labelIndex, fineIndex, proj)
    new GrammarRefinements(labels, rules)
  }
}
