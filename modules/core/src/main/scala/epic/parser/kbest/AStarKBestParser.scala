package epic.parser
package kbest

import epic.parser.projections.{AnchoredRuleMarginalProjector, ChartProjector}
import epic.trees.BinarizedTree

/**
 * Uses Top Down KBest A* (as implemented in [[epic.parser.kbest.TopDownKBestAStar]]) to generate
 * kbest lists.
 * @author dlwh
 */
class AStarKBestParser[L, W](val parser: Parser[L, W],
                        val decoder: ChartProjector[L, W] = new AnchoredRuleMarginalProjector[L, W]()) extends KBestParser[L, W] {
  def bestKParses(words: IndexedSeq[W], k: Int): IndexedSeq[(BinarizedTree[L], Double)] = {
    val maxMarginal = decoder.project(parser.marginal(words)).maxMarginal
    TopDownKBestAStar.apply(maxMarginal, k)
  }

}
