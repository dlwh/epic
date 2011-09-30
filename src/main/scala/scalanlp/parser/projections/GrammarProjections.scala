package scalanlp.parser
package projections

import scalanlp.util.Index

/**
 * 
 * @author dlwh
 */

@SerialVersionUID(1)
case class GrammarProjections[C,F](labels: ProjectionIndexer[C,F], rules: ProjectionIndexer[Rule[C],Rule[F]]) {
  def compose[F2](other: GrammarProjections[F,F2]) = new GrammarProjections(labels compose other.labels, rules compose other.rules)

}

object GrammarProjections {
  def identity[L](grammar: Grammar[L]): GrammarProjections[L, L] = {
    apply(grammar, grammar, Predef.identity[L] _)
  }

  def apply[C,F](coarse: Grammar[C], fine: Grammar[F], proj: F=>C): GrammarProjections[C, F] = {
    def projRule(r: Rule[F]) = r match {
      case BinaryRule(a,b,c) => BinaryRule(proj(a),proj(b),proj(c))
      case UnaryRule(a,b) => UnaryRule(proj(a),proj(b))
    }
    val rules = ProjectionIndexer(coarse.index,fine.index, projRule)
    val labels = ProjectionIndexer(coarse.labelIndex,fine.labelIndex, proj)

    new GrammarProjections(labels,rules)
  }

  def apply[C,F](coarse: Grammar[C], split: C=>Seq[F], proj: F=>C) = {
    def splitRule(r: Rule[C]) = r match {
      case BinaryRule(a,b,c) => for(a_ <- split(a); b_ <- split(b); c_ <- split(c)) yield BinaryRule(a_,b_,c_)
      case UnaryRule(a,b) => for(a_ <- split(a); b_ <- split(b)) yield UnaryRule(a_,b_)
    }
    val fineIndex = {
      val index = Index[F]();
      for( l <- coarse.labelIndex; l2 <- split(l)) {
        index.index(l2)
      }
      index;
    }
    val ruleIndex = {
      val index = Index[Rule[F]]()
      for( r <- 0 until coarse.index.size; split <- splitRule(coarse.index.get(r))) {
        index.index(split)
      }
      index
    }

    def projRule(r: Rule[F]) = r match {
      case BinaryRule(a,b,c) => BinaryRule(proj(a),proj(b),proj(c))
      case UnaryRule(a,b) => UnaryRule(proj(a),proj(b))
    }

    val rules = ProjectionIndexer(coarse.index,ruleIndex, projRule)
    val labels = ProjectionIndexer(coarse.labelIndex,fineIndex, proj)
    new GrammarProjections(labels,rules)
  }
}