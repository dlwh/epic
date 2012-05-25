package scalanlp.parser
package models

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class EPParserTest extends ParserTestHarness with FunSuite {

  test("basic test") {
    val factory = ParserTestHarness.simpleParser.augmentedGrammar
    val product = EPParser.fromChartParsers(factory.grammar,
      factory.lexicon, factory.core, factory.refined)

    val rprod = evalParser(getTestTrees(), product)
    assert(rprod.f1 > 0.6, rprod)
  }

  test("two parsers test") {
    val factory = ParserTestHarness.simpleParser.augmentedGrammar
    val product = EPParser.fromChartParsers(factory.grammar,
      factory.lexicon, factory.core, factory.refined, factory.refined)

    val rprod = evalParser(getTestTrees(), product)
    assert(rprod.f1 > 0.6, rprod)
  }
}

