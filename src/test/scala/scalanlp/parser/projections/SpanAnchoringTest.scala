package scalanlp.parser
package projections

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import scalanlp.trees.{TreeInstance, AnnotatedLabel}


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SpanAnchoringTest extends ParserTestHarness with FunSuite {

  test("We can parse using span anchoring") {
    val gen = ParserTestHarness.simpleParser
    val genFactory = gen.augmentedGrammar
    val f = new LabeledSpanProjector[AnnotatedLabel, String](genFactory.grammar, Double.NegativeInfinity)

    val grammar = new ProjectingCoreGrammar(gen.augmentedGrammar, f)
    val chartParser = SimpleChartParser(AugmentedGrammar.fromCore(grammar))

    for (TreeInstance(_, t, w) <- getTestTrees()) try {
      chartParser(w)
    } catch {
      case e: Exception =>
        throw new RuntimeException("Trouble with " + t.render(w), e)
    }

  }

  test("Parsing kind of works using it") {
    val gen = ParserTestHarness.simpleParser
    val f = new LabeledSpanProjector[AnnotatedLabel, String](gen.grammar, Double.NegativeInfinity)
    val grammar = new ProjectingCoreGrammar(gen.augmentedGrammar, f)

    val chartParser = SimpleChartParser(AugmentedGrammar.fromCore(grammar))

    val res = evalParser(getTestTrees(), chartParser)
    assert(res.f1 > 0.5, res.f1)

  }

}