package scalanlp.parser
package discrim

import projections._
import scalanlp.trees.UnaryChainRemover.ChainReplacer
;
import ParseChart._;

import scalanlp.trees._
import collection.mutable.ArrayBuffer
import java.io.{FileInputStream, BufferedInputStream, ObjectInputStream, File}

class ProductParser[L,L2,W](val parsers: Seq[CKYChartBuilder[LogProbabilityParseChart,L2,W]], coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
                       val projections: Seq[ProjectionIndexer[L,L2]]) extends Parser[L,W] {
  def bestParse(words: scala.Seq[W], spanScorer: SpanScorer[L]) = {
    val projected = projections.map(projectCoarseScorer(spanScorer,_));
    val posteriorScorers = for {
      (p,scorer) <- anchoredProjectors zip projected
    } yield {
      p.mkSpanScorer(words,scorer);
    }

    val sumScorer = posteriorScorers.foldLeft(spanScorer)(SpanScorer.sum(_:SpanScorer[L],_:SpanScorer[L]));


    val zeroInside = zeroParser.buildInsideChart(words,sumScorer);
    val zeroOutside = zeroParser.buildOutsideChart(zeroInside,sumScorer);
    val tree = SimpleViterbiDecoder(zeroGrammar).extractBestParse(coarseParser.root,zeroGrammar, zeroInside,zeroOutside, sumScorer);

    tree;
  }

  def projectCoarseScorer(coarseScorer: SpanScorer[L], projections: ProjectionIndexer[L,L2]):SpanScorer[L2] ={
    new ProjectingSpanScorer(projections, coarseScorer);
  }

  val anchoredProjectors = parsers zip projections map { case (parser,projection) =>
    new AnchoredRulePosteriorScorerFactory(parser,projection,Double.NegativeInfinity);
  }



  val zeroGrammar = new ZeroGrammar(coarseParser.grammar);
  val zeroLexicon = new ZeroLexicon(coarseParser.lexicon);
  val zeroParser = new CKYChartBuilder[LogProbabilityParseChart,L,W](coarseParser.root, zeroLexicon,zeroGrammar,ParseChart.logProb);
  val coarseIndexer = ProjectionIndexer(coarseParser.grammar.index, coarseParser.grammar.index, identity[L] _)
}

/**
 * 
 * @author dlwh
 */
object ProductParserRunner extends ParserTrainer {

  case class Params(parser: ParserParams.BaseParser, model0: File = null, model1: File = null, model2: File = null, model3: File = null);
  protected val paramManifest = manifest[Params];

  def trainParser(trainTrees: Seq[(BinarizedTree[String], Seq[String], SpanScorer[String])],
                  devTrees: Seq[(BinarizedTree[String], Seq[String], SpanScorer[String])],
                  unaryReplacer : ChainReplacer[String],
                  params: Params) = {
    val parsers = new ArrayBuffer[EPParser[String,(String,Int),String]];
    var found = true;
    var i = 0;
    val paths = params.productIterator;
    while(found && paths.hasNext) {
      found = false;
      val path = paths.dropWhile(!_.isInstanceOf[File]).next.asInstanceOf[File];
      println(path);
      if(path ne null) {
        parsers += readObject(path);
        found = true;
      }
      i += 1;
    }
    val coarseParser = params.parser.optParser;

    val productParser = new ProductParser(parsers.flatMap(_.parsers), coarseParser.get, parsers.flatMap(_.projections));
    Iterator.single( "Product" -> productParser);
  }


  def readObject[T](loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[T]
    oin.close();
    parser;
  }

}