package scalanlp.parser.epic

import scalala.tensor.dense._
import scalala.tensor.mutable.Counter2
import scalanlp.trees.{BinaryRule, UnaryRule}
import scalanlp.parser.projections.GrammarRefinements
import scalanlp.parser.{Lexicon, WeightedGrammar, Grammar}

object FeaturizedGrammar {
  def apply[L, L2, W](xbar: Grammar[L],
                      refinements: GrammarRefinements[L, L2],
                      weights: DenseVector[Double],
                      features: FeatureIndexer[L, L2, W],
                      lexicon: Lexicon[L2, W]) = {
    val ruleCache = new Array[Double](refinements.rules.fineIndex.size)
    val spanCache = new Array[Double](refinements.labels.fineIndex.size)

    for( (feats, r) <- features.ruleCache.iterator.zipWithIndex) {
      ruleCache(r) = feats dot weights;
    }

    WeightedGrammar.refined(xbar, refinements, ruleCache, spanCache, lexicon)
  }
}
