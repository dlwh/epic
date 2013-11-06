package epic.parser

import org.scalatest.FunSuite
import breeze.util.Implicits._

/**
 * 
 * @author dlwh
 */
class LatentTreeMarginalTest extends FunSuite {
  test("LatentTreeMarginal is the same as TreeMarginal with no refinements") {
    val trees = ParserTestHarness.getTrainTrees()
    for(t <- trees) {
      val augmented = AugmentedGrammar.fromRefined(ParserTestHarness.refinedGrammar)
      val lmarg = LatentTreeMarginal(augmented, t.words, t.tree.map(_ -> IndexedSeq(0)))
      val marg = TreeMarginal(augmented, t.words, t.tree.map(_ -> 0))
      assert(lmarg.logPartition closeTo marg.logPartition, lmarg.logPartition + " " +marg.logPartition)
      val lcounts = lmarg.expectedRuleCounts
      val counts = lmarg.expectedRuleCounts
      assert((lcounts.counts - counts.counts).norm(2) < 1E-4 * lcounts.counts.length)
    }

  }

}
