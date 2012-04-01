package scalanlp.parser
package projections

import org.junit.runner.RunWith;
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._
import scalanlp.trees.AnnotatedLabel

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class LabeledSpanScorerTest extends ParserTestHarness with FunSuite {

  test("We can parse using span scorer") {
    val gen = ParserTestHarness.simpleParser;
    val projections = ProjectionIndexer.simple(gen.builder.grammar.labelIndex)
    val f = new LabeledSpanScorerFactory(gen, Double.NegativeInfinity);
    for( TreeInstance(_, t, w, _) <- getTestTrees()) try {
      val gent = gen(w);
      val scorer = f.mkSpanScorer(w);
      val ginside2 = gen.builder.buildInsideChart(w, scorer);
      lazy val goutside2 = gen.builder.buildOutsideChart(ginside2, scorer)
      val tree = SimpleViterbiDecoder(ParserTestHarness.simpleGrammar).extractBestParse(AnnotatedLabel.TOP, gen.builder.grammar, ginside2, goutside2, w)
    } catch {
      case e: Exception =>
      throw new RuntimeException("Trouble with " + t.render(w), e);
    }

  }

}