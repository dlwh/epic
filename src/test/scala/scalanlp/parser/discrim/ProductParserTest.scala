package scalanlp.parser
package discrim

import org.junit.runner.RunWith;
import org.scalatest._;
import org.scalatest.junit._
import projections.{GrammarProjections, ProjectionIndexer}
;


/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class ProductParserTest extends ParserTestHarness with FunSuite {

  test("basic test") {
    val (trainTrees,replacer)= getTrainTreesAndReplacer();

    val gen = ParserTestHarness.simpleParser.builder.withCharts(ParseChart.logProb)
    val product = new ProductParser(Seq(gen),gen,Seq(GrammarProjections.identity(gen.grammar)))

    val rprod = evalParser(getTestTrees(),product)
    assert(rprod.f1 > 0.6, rprod);
  }

  test("two parsers test") {
    val (trainTrees,replacer)= getTrainTreesAndReplacer();

    val gen = ParserTestHarness.simpleParser.builder.withCharts(ParseChart.logProb)
    val product = new ProductParser(Seq(gen,gen),gen,Seq(GrammarProjections.identity(gen.grammar)))

    val rprod = evalParser(getTestTrees(),product)
    assert(rprod.f1 > 0.6, rprod);
  }

}

