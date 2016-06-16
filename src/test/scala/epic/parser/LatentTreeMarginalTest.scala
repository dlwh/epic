package epic.parser

import breeze.linalg.norm
import org.scalatest.FunSuite
import breeze.util.Implicits._
import breeze.linalg.norm

/**
 *
 * @author dlwh
 */
class LatentTreeMarginalTest extends FunSuite {
  test("LatentTreeMarginal is the same as TreeMarginal with no refinements") {
    val trees = ParserTestHarness.getTrainTrees()
    for(t <- trees) {
      val lmarg = LatentTreeMarginal(ParserTestHarness.refinedGrammar, t.words, t.tree.map(l => IndexedSeq(l -> 0)))
      val marg = TreeMarginal(ParserTestHarness.refinedGrammar, t.words, t.tree.map(_ -> 0))
      assert(lmarg.logPartition closeTo marg.logPartition, lmarg.logPartition + " " +marg.logPartition)
      val lcounts = lmarg.expectedRuleCounts
      val counts = lmarg.expectedRuleCounts
      assert(norm(lcounts.counts - counts.counts, 2.0) < 1E-4 * lcounts.length)
    }

  }

}
