package scalanlp.parser.lex

import scalanlp.parser.ParserParams.NoParams
import scalala.library.Library
import scalanlp.parser._

/**
 * 
 * @author dlwh
 */

object GenLexPipeline extends ParserPipeline with NoParams {
  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]],
                  validate: Parser[String, String]=>ParseEval.Statistics,
                  config: Params) = {
    val (words, binary, unary) = GenerativeParser.extractCounts(trainTrees);
    val grammar = Grammar(Library.logAndNormalizeRows(binary), Library.logAndNormalizeRows(unary));
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
    val lexgram = new LexGrammar[String, String] {
      def index = grammar.index

      def labelIndex = grammar.labelIndex

      def rulesForLabel(label: Int) = grammar.indexedBinaryRulesWithParent(label)

      val log2 = math.log(2)
      def scoreLeftComplement(rule: Int, words: Seq[String], head: Int, leftHead: Int) = {
        grammar.ruleScore(rule) - log2
      }

      def scoreRightComplement(rule: Int, words: Seq[String], head: Int, rightHead: Int) = {
        grammar.ruleScore(rule) - log2
      }

      def scoreUnary(rule: Int, words: Seq[String], head: Int) = {
        grammar.ruleScore(rule)
      }

      def tagScores(words: Seq[String], head: Int) = {
        grammar.labelEncoder.encodeOldSparse(lexicon.tagScores(words(head)), default=Double.NegativeInfinity)
      }

      def maxNumBinaryRulesForParent = grammar.maxNumBinaryRulesForParent

      val indexedRules = grammar.indexedRules

      def ruleIndex(a: Int, b: Int, c: Int) = grammar.ruleIndex(a, b, c)

      def ruleIndex(a: Int, b: Int) = grammar.ruleIndex(a, b)

      def indexedBinaryRulesWithParent(l: Int) = grammar.indexedBinaryRulesWithParent(l)

      def indexedBinaryRulesWithLeftChild(b: Int) = grammar.indexedBinaryRulesWithLeftChild(b)

      def indexedBinaryRulesWithRightChild(c: Int) = null

      def indexedUnaryRulesWithChild(l: Int) = grammar.indexedUnaryRulesWithChild(l)

      def indexedUnaryRulesWithParent(l: Int) = grammar.indexedUnaryRulesWithParent(l)
    }
    val parser = new LexChartParser(grammar, new LexCKYChartBuilder("",lexgram, ParseChart.logProb))
    Iterator.single(("Gen", parser));
  }

}