package scalanlp.parser
package epic

import org.junit.runner.RunWith;
import org.scalatest._;
import org.scalatest.junit._
import projections.{GrammarRefinements, ProjectionIndexer}


/**
 *
 * @author dlwh
@RunWith(classOf[JUnitRunner])
class EPParserTest extends ParserTestHarness with FunSuite {

  test("basic test") {

    val product = EPParser.fromChartParsers(ParserTestHarness.simpleParser, ParserTestHarness.simpleParser)

    val rprod = evalParser(getTestTrees(), product)
    println(rprod, evalParser(getTestTrees(), ParserTestHarness.simpleParser));
    assert(rprod.f1 > 0.6, rprod);
  }

  test("two parsers test") {

    val product = EPParser.fromChartParsers(ParserTestHarness.simpleParser,
      ParserTestHarness.simpleParser,
      ParserTestHarness.simpleParser)

    val rprod = evalParser(getTestTrees(), product)
    println(rprod, evalParser(getTestTrees(), ParserTestHarness.simpleParser));
    assert(rprod.f1 > 0.6, rprod);
  }

}
 */

