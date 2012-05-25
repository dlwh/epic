package scalanlp.parser.models

import scalala.tensor.dense._
import scalanlp.trees.{BinaryRule, UnaryRule}
import scalanlp.parser.projections.GrammarRefinements
import scalanlp.parser.{TagScorer, Lexicon, RefinedGrammar, BaseGrammar}

object FeaturizedGrammar {
  def apply[L, L2, W](xbar: BaseGrammar[L],
                      lexicon: Lexicon[L, W],
                      refinements: GrammarRefinements[L, L2],
                      weights: DenseVector[Double],
                      features: FeatureIndexer[L, L2, W],
                      tagScorer: TagScorer[L2, W]) = {
    val ruleCache = Array.tabulate[Double](refinements.rules.fineIndex.size){r =>
      features.computeWeight(r,weights)
    }
    val spanCache = new Array[Double](refinements.labels.fineIndex.size)

    RefinedGrammar.unanchored(xbar, lexicon, refinements, ruleCache, spanCache, tagScorer)
  }
}
