package scalanlp.parser
package projections

import org.junit.runner.RunWith;
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class LabeledSpanScorerTest extends ParserTestHarness with FunSuite {

  test("We can parse using span scorer") {
    val gen = ParserTestHarness.simpleParser;
    val projections = new ProjectionIndexer(gen.builder.grammar.index,gen.builder.grammar.index,identity[String])
    val f = new LabeledSpanScorerFactory(gen.builder.withCharts(ParseChart.logProb), projections);
    for( (t,w) <- getTestTrees()) try {
      val gent = gen(w);
      val scorer = f.mkSpanScorer(w);
      val ginside2 = gen.builder.buildInsideChart(w,scorer);
      lazy val goutside2 = gen.builder.buildOutsideChart(ginside2,scorer)._1
      val tree = SimpleViterbiDecoder(ParserTestHarness.simpleGrammar).extractBestParse("",gen.builder.grammar, ginside2,goutside2,null)
    } catch {
      case e: Exception =>
      throw new RuntimeException("Trouble with " + t.render(w),e);
    }

  }

}