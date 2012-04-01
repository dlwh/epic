package scalanlp.parser.lex

import scalanlp.parser.ParserParams.NoParams
import scalala.library.Library
import scalanlp.parser._
import projections.GrammarProjections
import scalanlp.util.Index
import collection.immutable.BitSet
import scalanlp.trees.AnnotatedLabel


/**
 *  Just makes sure we can parse with a non-lexicalized grammar/lexicon using the lexicalized parser
 * @author dlwh
 */
object SanityCheckLexPipeline extends ParserPipeline with NoParams {
  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  validate: Parser[AnnotatedLabel, String]=>ParseEval.Statistics,
                  config: Params) = {
    val trees = trainTrees.map(_.mapLabels(_.baseAnnotatedLabel))
    val (words, binary, unary) = GenerativeParser.extractCounts(trees);
    val grammar = Grammar(Library.logAndNormalizeRows(binary), Library.logAndNormalizeRows(unary));
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
    val _wordIndex = Index(words.keysIterator.map(_._2))
    val lexgram = new LexGrammar[AnnotatedLabel, String] {
      def index = grammar.index

      def labelIndex = grammar.labelIndex
      def wordIndex = _wordIndex

      def tags = lexicon.tags.toIndexedSeq
      val indexedTags = BitSet() ++ tags.map(grammar.labelIndex)

      val log2 = math.log(2)

      def specialize(w: Seq[String]) = new Specialization {
        val words = w

        def scoreLeftComplement(rule: Int, head: Int, leftHead: Int) = {
          grammar.ruleScore(rule) - log2
        }

        def scoreRightComplement(rule: Int, head: Int, rightHead: Int) = {
          grammar.ruleScore(rule) - log2
        }

        def scoreUnary(rule: Int, head: Int) = {
          grammar.ruleScore(rule)
        }


        def tagScores(head: Int) = {
          grammar.labelEncoder.encodeOldSparse(lexicon.tagScores(words(head)), default=Double.NegativeInfinity)
        }

      }

      def isLeftRule(r: Int) = true
      def isRightRule(r: Int) = true


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
    val parser = new LexMaxVChartParser(grammar, lexicon, GrammarProjections.identity(grammar),
      new LexCKYChartBuilder(AnnotatedLabel.TOP, lexgram, ParseChart.logProb))
    Iterator.single(("Gen", parser));
  }

}