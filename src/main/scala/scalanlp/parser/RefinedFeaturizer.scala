package scalanlp.parser

import projections.GrammarRefinements
import scalanlp.trees.{LexicalProduction, Production, Rule}
import scalanlp.util.Index

/**
 *
 * @author dlwh
 */

trait RefinedFeaturizer[L, W, Feat] {
  def index: Index[Feat]
  
  def specialize(words: Seq[W]):Anchoring
  
  trait Anchoring {
    def words: Seq[W]

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int):Array[Int]
    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int):Array[Int]
    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int):Array[Int]
  }

}

object RefinedFeaturizer {
  
  def forGrammar[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]):RefinedFeaturizer[L, W, Production[L, W]] = {
    forRefinedGrammar(grammar, lexicon, GrammarRefinements.identity(grammar))
  }
  
  def forRefinedGrammar[L, L2, W](coarse: BaseGrammar[L],
                                  lexicon: Lexicon[L, W],
                                  proj: GrammarRefinements[L, L2]):RefinedFeaturizer[L, W, Production[L2, W]] = {

    val refinedTagWords = for {
      LexicalProduction(l, w) <- lexicon.knownLexicalProductions
      l2 <- proj.labels.refinementsOf(l)
    } yield LexicalProduction(l2, w)

    val ind = Index[Production[L2,W]](proj.rules.fineIndex ++ refinedTagWords)
    
    val refinedRuleIndices = Array.tabulate(coarse.index.size) { r =>
      proj.rules.refinementsOf(r).map(fr => ind(proj.rules.fineIndex.get(fr)))
    }

    new RefinedFeaturizer[L, W, Production[L2, W]] {
      def index = ind

      def specialize(w: Seq[W]) = new Anchoring {
        val words = w

        def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
          Array(refinedRuleIndices(rule)(ref))
        }

        def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
          Array(refinedRuleIndices(rule)(ref))
        }

        def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
          if(begin+1 != end) Array.empty[Int]
          else {
            val refinedTag = proj.labels.globalize(tag, ref)
            val prod = LexicalProduction(proj.labels.fineIndex.get(refinedTag), words(begin))
            Array(ind(prod))
          }
        }
      }
    }
    
  }
}