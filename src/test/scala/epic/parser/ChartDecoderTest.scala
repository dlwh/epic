package epic.parser

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import epic.trees.AnnotatedLabel


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class ChartDecoderTest extends ParserTestHarness with FunSuite {

  test("ViterbiDecoder") {
    val gen = new SimpleChartParser(ParserTestHarness.simpleParser.augmentedGrammar, new ViterbiDecoder)

    val res = evalParser(getTestTrees(), gen)
    assert(res.f1 > 0.6, res.f1)
  }

  test("MaxRuleProductDecoder") {
    val factory = ParserTestHarness.simpleParser.augmentedGrammar
    val decoder = new MaxRuleProductDecoder[AnnotatedLabel, String](factory.grammar, factory.lexicon)
    val gen = new SimpleChartParser(factory, decoder)

    val res = evalParser(getTestTrees(), gen)
    assert(res.f1 > 0.6, res.f1)
  }

  test("MaxVariationalDecoder") {
    val factory = ParserTestHarness.simpleParser.augmentedGrammar
    val decoder = new MaxVariationalDecoder[AnnotatedLabel, String](factory.grammar, factory.lexicon)
    val gen = new SimpleChartParser(factory, decoder)

    val res = evalParser(getTestTrees(), gen)
    assert(res.f1 > 0.6, res.f1)
  }


  test("MaxConstituentDecoder") {
    val gen = new SimpleChartParser(ParserTestHarness.simpleParser.augmentedGrammar, new MaxConstituentDecoder)

    val res = evalParser(getTestTrees(), gen)
    assert(res.f1 > 0.6, res.f1)
  }

}