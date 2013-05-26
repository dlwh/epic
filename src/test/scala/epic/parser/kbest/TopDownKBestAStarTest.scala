package epic.parser.kbest

import epic.parser.{TreeMarginal, ViterbiDecoder, ParserTestHarness, SimpleChartParser}
import org.scalatest.FunSuite
import epic.trees.AnnotatedLabel

/**
 *
 * @author dlwh
 */
class TopDownKBestAStarTest extends ParserTestHarness with FunSuite {
  test("KBest recovers viterbi tree") {
    val kbestParser = new AStarKBestParser(ParserTestHarness.simpleParser.augmentedGrammar)
    val parser = new SimpleChartParser(ParserTestHarness.simpleParser.augmentedGrammar)
    val trees = getTestTrees()
    trees.foreach { ti =>
      val vit = parser.bestParse(ti.words)
      val kbest = kbestParser.bestKParses(ti.words, 5)
      assert(kbest.head._1 === vit, kbest)
      assert(kbest.sliding(2).forall(seq => seq.head._2 >= seq.last._2))

    }
  }

}
