package scalanlp.parser
package projections

import org.junit.runner.RunWith;
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class CoarseToFineTest extends ParserTestHarness with FunSuite {

  test("coarse2fine parser shouldn't be lossy wrt generative parser") {
    val (trainTrees,replacer)= getTrainTreesAndReplacer();
    def proj(label: String) =  if(label == "" ) label else "X";
    val coarseTrees = for {
      (tree,words) <- trainTrees
    } yield (tree map proj, words);

    val coarse = GenerativeParser.fromTrees(coarseTrees);
    val coarseBuilder = coarse.builder.asInstanceOf[ChartBuilder[scalanlp.parser.ParseChart.ViterbiParseChart,String,String]];

    val (fineLexicon,fineGrammar) = GenerativeParser.extractLexiconAndGrammar(trainTrees.iterator);
    val fine = new CoarseToFineChartBuilder[ParseChart.ViterbiParseChart,String,String,String](coarseBuilder, proj _, "", fineLexicon, fineGrammar, ParseChart.viterbi);
    val parser = ChartParser(fine);
    val gen = ParserTestHarness.simpleParser

    val rctf = evalParser(getTestTrees(),parser)
    val rgen = evalParser(getTestTrees(),gen)
    assert(rctf === rgen);
    //assert(rgen._4 > 0.6, rgen);

  }

}