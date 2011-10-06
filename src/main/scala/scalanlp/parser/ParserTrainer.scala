package scalanlp.parser;

import projections.ProjectTreebankToLabeledSpans
import scalanlp.config._
import java.io._
import scalanlp.trees._
import scalanlp.util._
import scalanlp.trees.UnaryChainRemover.ChainReplacer


/**
 * Mostly a utility class for parsertrainers.
 */
object ParserParams {
  case class Params();
  trait NoParams { self: ParserTrainer =>
    type Params = ParserParams.Params;
    protected val paramManifest = manifest[Params];
  }

  case class BaseParser(base: File = null) {
    def optParser = Option(base).map { f =>
     readObject[ChartParser[String,String,String]](f).builder.withCharts(ParseChart.logProb)
    }
  }

}

/**
 * ParserTrainer is a base-trait for the parser training pipeline. Handles
 * reading in the treebank and params and such
 */
trait ParserTrainer {
  /**
   * The type of the parameters to read in via scalanlp.config
   */
  type Params;
  /**
   * Required manifest for the params
   */
  protected implicit val paramManifest: Manifest[Params];

  /**
   * The main point of entry for implementors. Should return a sequence
   * of parsers
   */
  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees: IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  params: Params):Iterator[(String,Parser[String,String])];


  def trainParser(treebank: ProcessedTreebank, params: Params):Iterator[(String,Parser[String,String])] = {
    import treebank._;

    val parsers = trainParser(trainTrees,devTrees,replacer,params);
    parsers
  }

  /**
   * Trains a sequence of parsers and evaluates them.
   */
  def main(args: Array[String]) {
    val (baseConfig,files) = scalanlp.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[ProcessedTreebank]("parser");
    val specificParams = config.readIn[Params]("trainer");
    println("Training Parser...");
    println(params);
    println(specificParams);

    val parsers = trainParser(params,specificParams);

    import params._;

    for((name,parser) <- parsers) {
      println("Parser " + name);

      println("Evaluating Parser...");
      val stats = evalParser(testTrees,parser,name,replacer);
      import stats._;
      println("Eval finished. Results:");
      println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tag Accuracy: " + tagAccuracy);
      val outDir = new File("parsers/");
      outDir.mkdirs();
      val out = new File(outDir,name +".parser")
      writeObject(out,parser);
    }
  }

  def evalParser(testTrees: IndexedSeq[TreeInstance[String,String]],
          parser: Parser[String,String], name: String, chainReplacer: ChainReplacer[String]):ParseEval.Statistics = {
    val stats = ParseEval.evaluateAndLog(testTrees,parser,name,chainReplacer);
    stats
  }

}
