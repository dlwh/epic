package epic.parser
package projections

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import epic.trees.{TreeInstance, AnnotatedLabel}


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SimpleAnchoringTest  extends ParserTestHarness with FunSuite {

  test("We can parse using simple anchoring") {
    val gen = ParserTestHarness.simpleParser
    val genFactory = gen.augmentedGrammar
    val f = new AnchoredPCFGProjector[AnnotatedLabel, String](genFactory.grammar, Double.NegativeInfinity)

    val grammar = new ProjectingCoreGrammar(gen.augmentedGrammar, f)
    val chartParser = SimpleChartParser(AugmentedGrammar.fromCore(grammar))

    val grammarNext = new ProjectingCoreGrammar(chartParser.augmentedGrammar, f)
    val chartNext = SimpleChartParser(AugmentedGrammar.fromCore(grammarNext))

    for( TreeInstance(_, t, w) <- getTestTrees()) try {
      val tree1 = chartParser(w)
      val tree2 = chartNext(w)
      assert(tree2 === tree1, "late")
    } catch {
      case e: Exception =>
      throw new RuntimeException("Trouble with " + t.render(w), e)
    }

  }

  test("Parsing kind of works using it") {
    val gen = ParserTestHarness.simpleParser
    val f = new AnchoredPCFGProjector[AnnotatedLabel, String](gen.grammar, Double.NegativeInfinity)
    val grammar = new ProjectingCoreGrammar(gen.augmentedGrammar, f)

    val chartParser = SimpleChartParser(AugmentedGrammar.fromCore(grammar))

    val res = evalParser(getTestTrees(), chartParser)
    assert(res.f1 > 0.5, res.f1)

  }

}