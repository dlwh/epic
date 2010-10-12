package scalanlp.parser

import scalala.tensor.counters.Counters.DoubleCounter
import scalanlp.config.Configuration
import scalanlp.trees._

import ChartParser._;

class CoarseToFineParser[Chart[X]<:ParseChart[X],C,F,W](coarseParser: ChartParser[Chart,C,W],
                                proj: F=>C,
                                val root: F,
                                val lexicon: Lexicon[F,W],
                                val grammar: Grammar[F],
                                chartFactory: ParseChart.Factory[Chart] = ParseChart.viterbi,
                                threshold:Double = -100) extends ChartParser[Chart,F,W] {

  private val indexedProjections = grammar.fillArray(-1);
  for( (l,idx) <- grammar.index.zipWithIndex) {
    indexedProjections(idx) = coarseParser.grammar.index(proj(l));
  }
  private val coarseRootIndex = coarseParser.grammar.index(proj(root));

  private val fineParser = new CKYParser[Chart,F,W](root,lexicon,grammar,chartFactory);

  def buildInsideChart(s: Seq[W], validSpan: SpanFilter = defaultFilter):Chart[F] = {
    val coarseInside = coarseParser.buildInsideChart(s, validSpan)
    val coarseOutside = coarseParser.buildOutsideChart(coarseInside, validSpan);

    val sentProb = coarseInside(0,s.length,coarseRootIndex);
    println(sentProb);
    assert(!sentProb.isInfinite, s);

    def spanFilter(begin: Int, end: Int, label: Int) = (
      if(!validSpan(begin,end,label)) false
      else {
        val score =  (coarseInside(begin,end,indexedProjections(label))
                + coarseOutside(begin,end,indexedProjections(label)) - sentProb);
        score > threshold;
      }
    );

    fineParser.buildInsideChart(s,spanFilter _);
  }

    /**
   * Given an inside chart, fills the passed-in outside parse chart with inside scores.
   */
  def buildOutsideChart(inside: ParseChart[F],
                        validSpan: SpanFilter = defaultFilter):Chart[F] = {
    fineParser.buildOutsideChart(inside, validSpan);
  }

  def withCharts[Chart[X]<:ParseChart[X]](factory: ParseChart.Factory[Chart]) = {
    val cc = coarseParser.withCharts(factory);
    new CoarseToFineParser[Chart,C,F,W](cc,proj, root,lexicon,grammar,factory, threshold);
  }
  
}

object SimpleCoarseToFineTester extends ParserTester {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {
    def proj(label: String) =  if(label == "" ) label else "X";
    val coarseTrees = for {
      (tree,words) <- trainTrees
    } yield (tree map proj, words);

    val coarse = GenerativeParser.fromTrees(coarseTrees);

    val (fineLexicon,fineGrammar) = GenerativeParser.extractLexiconAndGrammar(trainTrees.iterator);
    val fine = new CoarseToFineParser[ParseChart.ViterbiParseChart,String,String,String](coarse, proj _, "", fineLexicon, fineGrammar, ParseChart.viterbi);

    Iterator.single(("CtF", fine));
  }
}
