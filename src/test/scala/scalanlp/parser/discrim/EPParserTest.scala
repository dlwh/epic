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
class EPParserTest extends ParserTestHarness with FunSuite {

  test("basic test") {
    val (trainTrees,replacer)= getTrainTreesAndReplacer();

    val gen = ParserTestHarness.simpleParser.builder.withCharts(ParseChart.logProb)
    val product = new EPParser(Seq(gen),gen,Seq(GrammarProjections.identity(gen.grammar)),maxEPIterations = 2)

    val rprod = evalParser(getTestTrees(),product)
    println(rprod,evalParser(getTestTrees(),ParserTestHarness.simpleParser));
    assert(rprod.f1 > 0.6, rprod);
  }

  test("two parsers test") {
    val (trainTrees,replacer)= getTrainTreesAndReplacer();

    val gen = ParserTestHarness.simpleParser.builder.withCharts(ParseChart.logProb)
    val product = new EPParser(Seq(gen,gen),gen,Seq(GrammarProjections.identity(gen.grammar),GrammarProjections.identity(gen.grammar)),maxEPIterations = 10)

    val rprod = evalParser(getTestTrees(),product)
    println(rprod,evalParser(getTestTrees(),ParserTestHarness.simpleParser));
    assert(rprod.f1 > 0.6, rprod);
  }

}

