package epic.parser

import breeze.util.Index
import epic.trees.{Production, LexicalProduction}


/**
 * A simple Featurizer that just counts lexical and rule productions that are used.
 * @author dlwh
 */
class ProductionFeaturizer[L, W](grammar: BaseGrammar[L],
                                 lexicalProductions: Iterable[LexicalProduction[L, W]]) extends RefinedFeaturizer[L, W, Production[L,W]] {
  val index = {
    val index = Index[Production[L, W]]()
    grammar.index foreach {index.index(_)}
    lexicalProductions foreach {index.index(_)}
    index
  }

  def anchor(w: Seq[W]) = new Anchoring {
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
