package scalanlp.parser

import projections.{AnchoredRulePosteriorScorerFactory, ProjectionIndexer}
import scalanlp.trees._
import scalanlp.parser.ParserParams.NoParams
import scalanlp.trees.UnaryChainRemover.ChainReplacer


/**
 * A ChartDecoder can turn an inside chart (and optionally an outside chart) from some
 * parse chart over symbols F into a tree over symbols C
 *
 * @author dlwh
 */
@serializable
trait ChartDecoder[C,F,W] {
  def extractBestParse(root:F, grammar: Grammar[F],
                       inside: ParseChart[F],
                       outside: =>ParseChart[F], words: Seq[W],
                       spanScorer: SpanScorer[F] = SpanScorer.identity):BinarizedTree[C];

}

/**
 * Tries to extract a tree that maximizes log score.
 */
@serializable
@SerialVersionUID(1)
class ViterbiDecoder[C,F, W](val indexedProjections: ProjectionIndexer[C,F]) extends ChartDecoder[C,F,W] {

  override def extractBestParse(root: F, grammar: Grammar[F],
                                inside: ParseChart[F],
                                outside: =>ParseChart[F], words: Seq[W],
                                spanScorer: SpanScorer[F] = SpanScorer.identity):BinarizedTree[C] = {

    def buildTreeUnary(start: Int, end:Int, root: Int):BinarizedTree[C] = {
      var maxScore = Double.NegativeInfinity;
      var maxChild = -1;
      for {
        (b,ruleScore) <- grammar.unaryRulesByIndexedParent(root)
      } {
        val score = ruleScore + inside.bot(start,end,b) + spanScorer.scoreUnaryRule(start,end,root,b);
        if(score > maxScore) {
          maxScore = score;
          maxChild = b;
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(start,end).map { case (i,v) => (grammar.index.get(i),v)}.toList)
        error("Couldn't find a tree!" + start + " " + end + " " + grammar.index.get(root));
      }
      val child = buildTree(start,end,maxChild);
      UnaryTree(indexedProjections.coarseSymbol(root),child)(Span(start,end));
    }

    def buildTree(start: Int, end: Int, root: Int):BinarizedTree[C] = {
      var maxScore = Double.NegativeInfinity;
      var maxLeft = -1;
      var maxRight = -1;
      var maxSplit = -1;
      if(start +1 == end) {
        return NullaryTree(indexedProjections.coarseSymbol(root))(Span(start,end));
      }

      for {
        (b,rchild) <- grammar.allBinaryRules;
        (c,parentScores) <- rchild
        split <- inside.top.feasibleSpan(start, end, b, c)
      } {
        val ruleScore = parentScores(root);
        val score = ruleScore + inside.top.labelScore(start,split,b) +
                inside.top.labelScore(split,end,c) + spanScorer.scoreBinaryRule(start,split,end,root,b,c);
        if(score > maxScore) {
          maxScore = score;
          maxLeft = b;
          maxRight = c;
          maxSplit = split;
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(start,end).map { case (i,v) => (grammar.index.get(i),v)}.toList)
        error("Couldn't find a tree!" + start + " " + end + " " + grammar.index.get(root));
      } else {
        val lchild = buildTreeUnary(start,maxSplit,maxLeft);
        val rchild = buildTreeUnary(maxSplit,end,maxRight);
        BinaryTree(indexedProjections.coarseSymbol(root),lchild,rchild)(Span(start,end));
      }


    }

    buildTreeUnary(0,inside.length, grammar.index(root));
  }
}

@serializable
@SerialVersionUID(1)
class SimpleViterbiDecoder[L,W](grammar: Grammar[L]) extends
    ViterbiDecoder[L,L,W](ProjectionIndexer.simple(grammar.index));


object SimpleViterbiDecoder {
  def apply[L,W](g: Grammar[L]):SimpleViterbiDecoder[L,W] = new SimpleViterbiDecoder[L,W](g);
}

/**
 * Tries to extract a tree that maximizes rule sum in the coarse grammar
 **/
class MaxRuleSumDecoder[C,F, W](coarseGrammar: Grammar[C], coarseLexicon: Lexicon[C,W],
                             indexedProjections: ProjectionIndexer[C,F],
                             fineBuilder: ChartBuilder[ParseChart.LogProbabilityParseChart, F, W]) extends ChartDecoder[C,F, W] {
  val p = new AnchoredRulePosteriorScorerFactory(fineBuilder,indexedProjections,-5)

  override def extractBestParse(root: F, grammar: Grammar[F],
                                inside: ParseChart[F], outside: =>ParseChart[F], words:Seq[W],
                                spanScorer: SpanScorer[F] = SpanScorer.identity):BinarizedTree[C] = {
    val zeroGrammar = new ZeroGrammar(coarseGrammar);
    val zeroLexicon = new ZeroLexicon(coarseLexicon);
    val coarseRoot = indexedProjections.project(root)
    val zeroParser = new CKYChartBuilder[ParseChart.LogProbabilityParseChart,C,W](coarseRoot, zeroLexicon, zeroGrammar,ParseChart.logProb);
    val scorer = p.buildSpanScorer(inside, outside, inside.top.labelScore(0,inside.length,root),spanScorer)
    val zeroInside = zeroParser.buildInsideChart(words,scorer);
    val zeroOutside = zeroParser.buildOutsideChart(zeroInside,scorer);
    val tree = SimpleViterbiDecoder(zeroGrammar).extractBestParse(coarseRoot,zeroGrammar, zeroInside,zeroOutside, words, scorer);
    tree
  }
}

import scalala.tensor.counters.LogCounters._;

object MaxRuleTrainer extends ParserTrainer with NoParams {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer[String])],
                  unaryReplacer : ChainReplacer[String],
                  config: Params) = {
    val (words,binary,unary) = GenerativeParser.extractCounts(trainTrees.iterator.map{ case (a,b,c) => (a,b)});
    val grammar = new GenerativeGrammar(logNormalizeRows(binary),logNormalizeRows(unary));
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 3);
    val projections = ProjectionIndexer.simple(grammar.index);
    val builder = CKYChartBuilder("",lexicon,grammar).withCharts(ParseChart.logProb);
    val decoder = new MaxRuleSumDecoder(grammar,lexicon,projections,builder)
    val parser = new ChartParser(builder,decoder,projections);
    Iterator.single(("Gen",parser));
  }
}
