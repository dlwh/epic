package epic.parser
package kbest

import epic.trees.BinarizedTree
import epic.parser.projections.{AnchoredRuleMarginalProjector, ChartProjector}
import epic.util.CacheBroker

/**
 * TODO
 * @author dlwh
 * @tparam L
 * @tparam W
 */
trait KBestParser[L, W] {
  def bestKParses(words: IndexedSeq[W], k: Int): IndexedSeq[(BinarizedTree[L], Double)]
}

object KBestParser {
  def fromParser[L, W](parser: SimpleChartParser[L, W], proj: ChartProjector[L, W] = AnchoredRuleMarginalProjector[L, W]()): KBestParser[L, W] = {
    apply(parser.augmentedGrammar, proj)
  }

  def apply[L, W](augmentedGrammar: AugmentedGrammar[L, W], proj: ChartProjector[L, W] = new AnchoredRuleMarginalProjector[L, W]()) = {
    new AStarKBestParser(augmentedGrammar, proj)
  }

  def cached[L, W](kbest: KBestParser[L, W])(implicit broker: CacheBroker):KBestParser[L, W] = new KBestParser[L, W] {
    val cache = broker.make[IndexedSeq[W], IndexedSeq[(BinarizedTree[L], Double)]]("epic.parser.kbest.KBestParser.cached")

    def bestKParses(words: IndexedSeq[W], k: Int): IndexedSeq[(BinarizedTree[L], Double)] = {
      cache.get(words) match {
        case Some(list) if list.length >= k => list.take(k)
        case _ =>
          val list = kbest.bestKParses(words, k)
          cache(words) = list
          list
      }
    }
  }
}
