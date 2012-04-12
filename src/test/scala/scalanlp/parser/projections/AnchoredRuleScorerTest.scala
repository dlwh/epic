package scalanlp.parser
package projections

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import scalanlp.trees.AnnotatedLabel


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class AnchoredRuleScorerTest  extends ParserTestHarness with FunSuite {

  test("We can parse using span scorer") {
    val gen = ParserTestHarness.simpleParser
    val genFactory = gen.builder.grammar
    val f = new AnchoredPCFGProjector[AnnotatedLabel, String](genFactory.grammar, Double.NegativeInfinity)

    val factory = new ProjectingScorerFactory(gen.builder.withCharts(ParseChart.logProb), f)
    val grammar = new SpanScorerGrammar(genFactory.grammar, genFactory.lexicon, factory)
    val chartParser = SimpleChartParser(grammar)

    val factoryNext = new ProjectingScorerFactory(chartParser.builder.withCharts(ParseChart.logProb), f)
    val grammarNext = new SpanScorerGrammar(genFactory.grammar, genFactory.lexicon, factoryNext)
    val chartNext = SimpleChartParser(grammarNext)

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
    val f = new AnchoredPCFGProjector[AnnotatedLabel, String](gen.builder.grammar.grammar, Double.NegativeInfinity)
    val factory = new ProjectingScorerFactory(gen.builder.withCharts(ParseChart.logProb), f)
    val grammar = new SpanScorerGrammar(gen.builder.grammar.grammar, gen.builder.grammar.lexicon, factory)

    val chartParser = SimpleChartParser(grammar)

    val res = evalParser(getTestTrees(), chartParser)
    assert(res.f1 > 0.5, res.f1)

  }

}