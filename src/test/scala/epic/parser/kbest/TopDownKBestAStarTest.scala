package epic.parser.kbest

import epic.parser.{ViterbiDecoder, ParserTestHarness}
import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
class TopDownKBestAStarTest extends FunSuite with ParserTestHarness {
  test("KBest recovers viterbi tree") {
    val parser = ParserTestHarness.viterbiParser
    val kbestParser = new AStarKBestParser(parser)
    val trees = getTestTrees()
    trees.foreach { ti =>
      val vit = parser.parse(ti.words)
      val kbest = kbestParser.bestKParses(ti.words, 5)
      assert(kbest.head._1 === vit, kbest)
      assert(kbest.sliding(2).forall(seq => seq.head._2 >= seq.last._2))

    }
  }

}
