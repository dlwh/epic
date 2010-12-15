package scalanlp.parser
package discrim

import projections._;
import ParseChart._;

import scalanlp.trees._
import scalanlp.config.Configuration
import collection.mutable.ArrayBuffer
import java.io.{FileInputStream, BufferedInputStream, ObjectInputStream, File}

class ProductParser[L,L2,W](val parsers: Seq[CKYChartBuilder[LogProbabilityParseChart,L2,W]], coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
                       val projections: Seq[ProjectionIndexer[L,L2]]) extends Parser[L,W] {
  def bestParse(words: scala.Seq[W], spanScorer: SpanScorer) = {
    val projected = projections.map(projectCoarseScorer(spanScorer,_));
    val posteriorScorers = for {
      (p,scorer) <- anchoredProjectors zip projected
    } yield {
      p.mkSpanScorer(words,scorer);
    }

    val sumScorer = posteriorScorers.foldLeft(spanScorer)(SpanScorer.sum(_:SpanScorer,_:SpanScorer));


    val zeroInside = zeroParser.buildInsideChart(words,sumScorer);
    val zeroOutside = zeroParser.buildOutsideChart(zeroInside,sumScorer);
    val tree = SimpleViterbiDecoder(zeroGrammar).extractBestParse(coarseParser.root,zeroGrammar, zeroInside,zeroOutside._1, zeroOutside._2, sumScorer);

    tree;
  }

  def projectCoarseScorer(coarseScorer: SpanScorer, projections: ProjectionIndexer[L,L2]):SpanScorer ={
    new ProjectingSpanScorer(projections, coarseScorer);
  }

  val anchoredProjectors = parsers zip projections map { case (parser,projection) =>
    new AnchoredPosteriorScorerFactory(parser,projection,Double.NegativeInfinity);
  }



  val zeroGrammar = new ZeroGrammar(coarseParser.grammar);
  val zeroLexicon = new ZeroLexicon(coarseParser.lexicon);
  val zeroParser = new CKYChartBuilder[LogProbabilityParseChart,L,W](coarseParser.root, zeroLexicon,zeroGrammar,ParseChart.logProb);
  val coarseIndexer = new ProjectionIndexer(coarseParser.grammar.index, coarseParser.grammar.index, identity[L] _)
}

/**
 * 
 * @author dlwh
 */
object ProductParserRunner extends ParserTrainer {
  def trainParser(trainTrees: Seq[(BinarizedTree[String], Seq[String], SpanScorer)],
                  devTrees: Seq[(BinarizedTree[String], Seq[String], SpanScorer)], config: Configuration) = {
    val parsers = new ArrayBuffer[EPParser[String,(String,Int),String]];
    var found = true;
    var i = 0;
    while(found) {
      found = false;
      val path = config.readIn[java.io.File]("parser.model" + i,null)
      println(path);
      if(path ne null) {
        parsers += readObject(path);
        found = true;
      }
      i += 1;
    }
    val coarseParser = loadParser(config);

    println(parsers);

    val productParser = new ProductParser(parsers.flatMap(_.parsers), coarseParser.get, parsers.flatMap(_.projections));
    Iterator.single( "Product" -> productParser);
  }


  def readObject[T](loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[T]
    oin.close();
    parser;
  }

  def loadParser(config: Configuration) = {
    val spanDir = config.readIn[File]("parser.base",null);
    if(spanDir eq null) None
    else {
      Some(ProjectTreebankToLabeledSpans.loadParser(spanDir).builder.withCharts(ParseChart.logProb))
    }
  }
}