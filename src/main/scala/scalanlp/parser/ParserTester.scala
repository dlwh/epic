package scalanlp.parser

import scalanlp.config.Configuration
import java.io.File
import scalanlp.util._
import scalanlp.trees.{TreeInstance, ProcessedTreebank, AnnotatedLabel, AnnotatedLabelChainReplacer}

/**
 * ParserTester just tests a grammar
 * reading in the treebank and params and such
 */
object ParserTester {
  /**
   * The type of the parameters to read in via scalanlp.config
   */
  case class Params(name: String, parser: File)

  /**
   * Trains a sequence of parsers and evaluates them.
   */
  def main(args: Array[String]) {
    val (baseConfig,files) = scalanlp.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[ProcessedTreebank]("parser");
    val specificParams = config.readIn[Params]("test");
    println("Evaluating Parser...");
    println(params);
    println(specificParams);

    val parser = readObject[Parser[AnnotatedLabel,String]](specificParams.parser)

    import params._;

    val name = specificParams.name

    println("Parser " + name);

    {
      println("Evaluating Parser on dev...");
      val stats = evalParser(devTrees,parser,name+ "-dev")
      import stats._;
      println("Eval finished. Results:");
      println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tag Accuracy: " + tagAccuracy);
    }

    {
      println("Evaluating Parser on test...");
      val stats = evalParser(testTrees,parser,name+ "-test")
      import stats._;
      println("Eval finished. Results:");
      println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tag Accuracy: " + tagAccuracy);
    }
  }

  def evalParser(testTrees: IndexedSeq[TreeInstance[AnnotatedLabel,String]],
          parser: Parser[AnnotatedLabel,String], name: String):ParseEval.Statistics = {
    ParseEval.evaluateAndLog(testTrees, parser, name, AnnotatedLabelChainReplacer)
  }

}