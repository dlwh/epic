package scalanlp.parser

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import scalanlp.trees.AnnotatedLabel


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class ChartDecoderTest extends ParserTestHarness with FunSuite {

  test("ViterbiDecoder") {
    val gen = new SimpleChartParser(ParserTestHarness.simpleParser.builder, new ViterbiDecoder)

    val res = evalParser(getTestTrees(), gen)
    assert(res.f1 > 0.5, res.f1)

  }

  test("MaxRuleProductDecoder") {
    val decoder = new MaxRuleProductDecoder[AnnotatedLabel, String](ParserTestHarness.simpleParser.builder.grammar.grammar)
    val gen = new SimpleChartParser(ParserTestHarness.simpleParser.builder, decoder)

    val res = evalParser(getTestTrees(), gen)
    assert(res.f1 > 0.5, res.f1)

  }

  test("MaxVariationalDecoder") {
    val decoder = new MaxVariationalDecoder[AnnotatedLabel, String](ParserTestHarness.simpleParser.builder.grammar.grammar)
    val gen = new SimpleChartParser(ParserTestHarness.simpleParser.builder, decoder)

    val res = evalParser(getTestTrees(), gen)
    assert(res.f1 > 0.5, res.f1)

  }


  test("MaxConstituentDecoder") {
    val gen = new SimpleChartParser(ParserTestHarness.simpleParser.builder, new MaxConstituentDecoder)

    val res = evalParser(getTestTrees(), gen)
    assert(res.f1 > 0.5, res.f1)

  }

}