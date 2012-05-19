package scalanlp.parser

import scalanlp.util.Index
import scalanlp.trees.{Production, LexicalProduction}


/**
 * A simple Featurizer that just counts lexical and rule productions that are used.
 * @author dlwh
 */
class ProductionFeaturizer[L, W](grammar: Grammar[L],
                                 lexicalProductions: Iterable[LexicalProduction[L, W]]) extends DerivationFeaturizer[L, W, Production[L,W]] {
  val index = {
    val index = Index[Production[L, W]]()
    grammar.index foreach {index.index(_)}
    lexicalProductions foreach {index.index(_)}
    index
  }

  def specialize(w: Seq[W]) = new Specialization {
    val words = w

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      Array(rule:Int)
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      Array(rule:Int)
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      if(begin + 1 != end)  Array.empty
      else Array(index(LexicalProduction(grammar.labelIndex.get(tag), words(begin))))
    }
  }
}
