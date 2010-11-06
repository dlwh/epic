package scalanlp.parser

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

  test("test on simpleGrammar") {
    val trainTrees = getTrainTrees();
    def proj(label: String) =  if(label == "" ) label else "X";
    val coarseTrees = for {
      (tree,words) <- trainTrees
    } yield (tree map proj, words);

    val coarse = GenerativeParser.fromTrees(coarseTrees);

    val (fineLexicon,fineGrammar) = GenerativeParser.extractLexiconAndGrammar(trainTrees.iterator);
    val fine = new CoarseToFineParser[ParseChart.ViterbiParseChart,String,String,String](coarse, proj _, "", fineLexicon, fineGrammar, ParseChart.viterbi);
    val gen = GenerativeParser.fromTrees(trainTrees.iterator);

    val rctf = evalParser(getTestTrees(),fine)
    val rgen = evalParser(getTestTrees(),gen)
    assert(rctf === rgen);

  }

}