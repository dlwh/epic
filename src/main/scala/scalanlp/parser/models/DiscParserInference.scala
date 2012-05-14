package scalanlp.parser.models

import scalanlp.epic.Feature
import scalanlp.parser.{TreeMarginal, DerivationScorer, DerivationFeaturizer}
import scalanlp.trees.{TreeInstance, BinarizedTree}

case class DiscParserInference[L, W](featurizer: DerivationFeaturizer[L, W, Feature],
                                     ann: (BinarizedTree[L], Seq[W]) => BinarizedTree[(L, Int)],
                                     grammar: DerivationScorer.Factory[L, W],
                                     baseMeasure: DerivationScorer.Factory[L, W]) extends ParserInference[L, W] {

  // E[T-z|T, params]
  def goldCounts(ti: TreeInstance[L, W], aug: DerivationScorer[L, W]) = {
    val tree = ti.tree
    val words = ti.words
    val annotated = ann(tree, words)


    val product = grammar.specialize(words) * aug

    TreeMarginal(product, annotated).expectedCounts(featurizer)
  }

}
