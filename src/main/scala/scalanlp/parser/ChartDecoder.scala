package scalanlp.parser


import java.util.Arrays;
import projections.{AnchoredRulePosteriorScorerFactory, ProjectionIndexer}
import scalanlp.trees._
import scalanlp.parser.ParserParams.NoParams
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalanlp.collection.mutable.TriangularArray
import scalanlp.util._;
import scalala.library.Numerics._
import scalala.library.Library


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
        (b,ruleScore) <- grammar.unaryRulesByIndexedParent(root).activeIterator
      } {
        val score = ruleScore + inside.bot(start,end,b) + spanScorer.scoreUnaryRule(start,end,root,b);
        if(score > maxScore) {
          maxScore = score;
          maxChild = b;
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(start,end).map { case (i,v) => (grammar.index.get(i),v)}.toList)
        sys.error("Couldn't find a tree!" + start + " " + end + " " + grammar.index.get(root));
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
        sys.error("Couldn't find a tree!" + start + " " + end + " " + grammar.index.get(root));
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
 * Tries to extract a tree that maximizes rule product in the coarse grammar
 **/
class MaxRuleProductDecoder[C,F, W](coarseGrammar: Grammar[C], coarseLexicon: Lexicon[C,W],
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

object MaxRuleTrainer extends ParserTrainer with NoParams {
  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees:   IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  config: Params) = {
    val (words,binary,unary) = GenerativeParser.extractCounts(trainTrees);
    val grammar = new GenerativeGrammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 3);
    val projections = ProjectionIndexer.simple(grammar.index);
    val builder = CKYChartBuilder("",lexicon,grammar).withCharts(ParseChart.logProb);
    val decoder = new MaxRuleProductDecoder(grammar,lexicon,projections,builder)
    val parser = new ChartParser(builder,decoder,projections);
    Iterator.single(("Gen",parser));
  }
}

class MaxConstituentDecoder[C,F,W](indexedProjections: ProjectionIndexer[C,F]) extends ChartDecoder[C,F,W] {
  override def extractBestParse(root: F, grammar: Grammar[F],
                                 inside: ParseChart[F], loutside: =>ParseChart[F], words: Seq[W],
                                 scorer : SpanScorer[F] = SpanScorer.identity):BinarizedTree[C] = {
    val outside = loutside;
    val maxSplit = new TriangularArray[Int](inside.length+1,0);
    val maxBotLabel = new TriangularArray[Int](inside.length+1,-1);
    val maxBotScore = new TriangularArray[Double](inside.length+1,Double.NegativeInfinity);
    val maxTopLabel = new TriangularArray[Int](inside.length+1,-1);
    val maxTopScore = new TriangularArray[Double](inside.length+1,Double.NegativeInfinity);

    val scores = indexedProjections.coarseEncoder.fillArray(Double.NegativeInfinity);
    for(i <- 0 until inside.length) {
      Arrays.fill(scores,Double.NegativeInfinity);
      for(l <- inside.bot.enteredLabelIndexes(i,i+1)) {
        val myScore = inside.bot.labelScore(i, i + 1, l) + outside.bot.labelScore(i, i+1, l);
        scores(indexedProjections.project(l)) = logSum(scores(indexedProjections.project(l)), myScore);
      }
      maxBotScore(i,i+1) = scores.max;
      maxBotLabel(i,i+1) = scores.argmax;

      Arrays.fill(scores,Double.NegativeInfinity);
      for(l <- inside.top.enteredLabelIndexes(i,i+1)) {
        val myScore = inside.top.labelScore(i, i + 1, l) + outside.top.labelScore(i, i+1, l);
        scores(indexedProjections.project(l)) = logSum(scores(indexedProjections.project(l)), myScore);
      }
      maxTopScore(i,i+1) = logSum(scores.max,maxBotScore(i,i+1));
      maxTopLabel(i,i+1) = scores.argmax;
    }

    for {
      span <- 2 to inside.length;
      begin <- 0 to (inside.length - span);
      end = begin + span
    } {
      Arrays.fill(scores,Double.NegativeInfinity);
      for(l <- inside.bot.enteredLabelIndexes(begin,end)) {
        val myScore = inside.bot.labelScore(begin, end, l) + outside.bot.labelScore(begin, end, l);
        scores(indexedProjections.project(l)) = logSum(scores(indexedProjections.project(l)), myScore);
      }
      maxBotScore(begin,end) = scores.max;
      maxBotLabel(begin,end) = scores.argmax;

      Arrays.fill(scores,Double.NegativeInfinity);
      for(l <- inside.top.enteredLabelIndexes(begin,end)) {
        val myScore = inside.top.labelScore(begin, end, l) + outside.top.labelScore(begin, end, l);
        scores(indexedProjections.project(l)) = logSum(scores(indexedProjections.project(l)), myScore);
      }
      maxTopScore(begin,end) = logSum(scores.max,maxBotScore(begin,end));
      maxTopLabel(begin,end) = scores.argmax;

      val (split,splitScore) = (for(split <- begin +1 until end) yield {
        val score = logSum(maxTopScore(begin,split),maxTopScore(split,end));
        (split,score)
      })reduceLeft ( (a,b) => if(a._2 > b._2) a else b);

      maxSplit(begin,end) = split;
      maxTopScore(begin,end) = logSum(maxTopScore(begin,end),splitScore);
      //maxBotScore(begin,end) = logSum(maxBotScore(begin,end),splitScore);
    }

    def extract(begin: Int, end: Int):BinarizedTree[C] = {
      val lower = if(begin + 1== end) {
        NullaryTree(indexedProjections.coarseIndex.get(maxBotLabel(begin,end)))(Span(begin,end));
      } else {
        val split = maxSplit(begin,end);
        val left = extract(begin,split);
        val right = extract(split,end);
        BinaryTree(indexedProjections.coarseIndex.get(maxBotLabel(begin,end)),left,right)(Span(begin,end));
      }

      UnaryTree(indexedProjections.coarseIndex.get(maxTopLabel(begin,end)),lower)(Span(begin,end));
    }

    extract(0,inside.length);


  }
}

object MaxConstituentTrainer extends ParserTrainer with NoParams {
  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees:   IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  config: Params) = {
    val (words,binary,unary) = GenerativeParser.extractCounts(trainTrees);
    val grammar = new GenerativeGrammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 3);
    val projections = ProjectionIndexer.simple(grammar.index);
    val builder = CKYChartBuilder("",lexicon,grammar).withCharts(ParseChart.logProb);
    val decoder = new MaxConstituentDecoder[String,String,String](projections)
    val parser = new ChartParser(builder,decoder,projections);
    Iterator.single(("Consti",parser));
  }
}
