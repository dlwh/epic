package scalanlp.parser

import scalanlp.util.{TypeTags, Index}
import scalanlp.trees.{Rule, Production, LexicalProduction}
import TypeTags._


/**
 *
 * @author dlwh
 */

class ProductionFeaturizer[L, W](grammar: Grammar[L],
                                 lexicalProductions: Iterable[LexicalProduction[L, W]]) extends SpanFeaturizer[L, W, Production[L,W]] {
  val index = {
    val index = Index[Production[L, W]]()
    grammar.index foreach {index.index(_)}
    lexicalProductions foreach {index.index(_)}
    index
  }

  def specialize(w: Seq[W]) = new Specialization {
    val words = w

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: ID[Rule[L]], ref: ID[RuleRef[L]]) = {
      Array(rule:Int)
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: ID[Rule[L]], ref: ID[RuleRef[L]]) = {
      Array(rule:Int)
    }

    def featuresForSpan(begin: Int, end: Int, tag: ID[L], ref: ID[Ref[L]]) = {
      Array(index(LexicalProduction(grammar.labelIndex.get(tag), words(begin))))
    }
  }
}
