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
    val gen = new SimpleChartParser(ParserTestHarness.simpleParser.augmentedGrammar, new ViterbiDecoder)
    val trees = getTestTrees()
    trees.foreach { ti =>
      val charts = gen.charts(ti.words)
      val vit = new ViterbiDecoder[AnnotatedLabel, String]().extractBestParse(charts)
      val kbest = TopDownKBestAStar.apply(charts, 5)
      assert(kbest.head._1 === vit, kbest)
      assert(kbest.sliding(2).forall(seq => seq.head._2 >= seq.last._2))

    }
  }

}
