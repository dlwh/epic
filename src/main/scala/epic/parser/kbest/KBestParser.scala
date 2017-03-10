package epic.parser
package kbest

import epic.trees.BinarizedTree
import epic.parser.projections.{AnchoredRuleMarginalProjector, ChartProjector}
import epic.util.CacheBroker
import breeze.util.SerializableLogging

/**
 * Produces a kbest list of parses, along with scores.
 * @author dlwh
 * @tparam L
 * @tparam W
 */
trait KBestParser[L, W] {
  def bestKParses(words: IndexedSeq[W], k: Int): IndexedSeq[(BinarizedTree[L], Double)]
}

object KBestParser extends SerializableLogging {
  def apply[L, W](parser: Parser[L, W], proj: ChartProjector[L, W] = new AnchoredRuleMarginalProjector[L, W]()) = {
    new AStarKBestParser(parser, proj)
  }

  def cached[L, W](kbest: KBestParser[L, W])(implicit broker: CacheBroker):KBestParser[L, W] = new KBestParser[L, W] {
    val cache = broker.make[IndexedSeq[W], IndexedSeq[(BinarizedTree[L], Double)]]("epic.parser.kbest.KBestParser.cached")

    def bestKParses(words: IndexedSeq[W], k: Int): IndexedSeq[(BinarizedTree[L], Double)] = {
      cache.get(words) match {
        case Some(list) if list.length >= k =>
          logger.debug(s"KBest cache for $words")
          list.take(k)
        case _ =>
          logger.info(s"Caching kbest list for $words")
          val list = kbest.bestKParses(words, k)
          cache(words) = list
          list
      }
    }
  }
}
