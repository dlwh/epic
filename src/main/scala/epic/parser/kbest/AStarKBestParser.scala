package epic.parser
package kbest

import epic.parser.projections.{AnchoredRuleMarginalProjector, ChartProjector}
import epic.trees.BinarizedTree


/**
 *
 * @author dlwh
 */
class AStarKBestParser[L, W](val augmentedGrammar: AugmentedGrammar[L, W],
                        val decoder: ChartProjector[L, W] = new AnchoredRuleMarginalProjector[L, W]()) extends KBestParser[L, W] {
  def bestKParses(words: IndexedSeq[W], k: Int): IndexedSeq[(BinarizedTree[L], Double)] = {
    val maxMarginal = decoder.project(augmentedGrammar.anchor(words).marginal).maxMarginal
    TopDownKBestAStar.apply(maxMarginal, k)
  }

}
