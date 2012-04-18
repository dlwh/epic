package scalanlp.parser

import projections.ConstraintScorerFactory
import scalanlp.config._
import java.io._
import scalanlp.trees._
import scalanlp.util._
import scalala.library.Library
import scalanlp.text.tokenize.EnglishWordClassGenerator


/**
 * Mostly a utility class for parsertrainers.
 */
object ParserParams {
  case class Params()
  trait NoParams { self: ParserPipeline =>
    type Params = ParserParams.Params
    protected val paramManifest = manifest[Params]
  }

  case class BaseParser(path: File = null) {
    def xbarGrammar(trees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = Option(path) match {
      case Some(f) =>
        readObject[(Grammar[AnnotatedLabel],Lexicon[AnnotatedLabel, String])](f)
      case None =>
        val (words, xbarBinaries, xbarUnaries) = GenerativeParser.extractCounts(trees.map(_.mapLabels(_.baseAnnotatedLabel)))

        val g = Grammar(AnnotatedLabel.TOP, xbarBinaries.keysIterator.map(_._2) ++ xbarUnaries.keysIterator.map(_._2))
        val lex = new SignatureLexicon(words, EnglishWordClassGenerator)
        g -> lex

    }
  }

  case class Constraints[L, W](path: File = null) {
    def cachedFactory(baseFactory: DerivationScorer.Factory[L, W], threshold: Double = -7) = {
      if(constraintsCache.contains(path)) {
        constraintsCache(path).asInstanceOf[DerivationScorer.Factory[L, W]]
      } else {
        val uncached = if(path eq null) {
          new ConstraintScorerFactory[L,W](ChartBuilder(baseFactory), threshold)
        }
        else {
          val constraint = new ConstraintScorerFactory[L,W](ChartBuilder(baseFactory), threshold)
          new FileCachedScorerFactory(constraint, path)
        }

        constraintsCache(path) = uncached
        uncached
      }

    }
  }
  
  private val constraintsCache = new MapCache[File, DerivationScorer.Factory[_, _]]
}

/**
 * ParserTrainer is a base-trait for the parser training pipeline. Handles
 * reading in the treebank and params and such
 */
trait ParserPipeline {
  /**
   * The type of the parameters to read in via scalanlp.config
   */
  type Params
  /**
   * Required manifest for the params
   */
  protected implicit val paramManifest: Manifest[Params]

  /**
   * The main point of entry for implementors. Should return a sequence
   * of parsers
   */
  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  validate: Parser[AnnotatedLabel, String]=>ParseEval.Statistics,
                  params: Params):Iterator[(String, Parser[AnnotatedLabel, String])]


  def trainParser(treebank: ProcessedTreebank, params: Params):Iterator[(String, Parser[AnnotatedLabel, String])] = {
    import treebank._


    val validateTrees = devTrees.take(400)
    def validate(parser: Parser[AnnotatedLabel, String]) = {
      ParseEval.evaluate[AnnotatedLabel](validateTrees, parser, AnnotatedLabelChainReplacer, asString={(l:AnnotatedLabel)=>l.label})
    }
    val parsers = trainParser(trainTrees, validate, params)
    parsers
  }

  /**
   * Trains a sequence of parsers and evaluates them.
   */
  def main(args: Array[String]) {
    val (baseConfig, files) = scalanlp.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[ProcessedTreebank]("treebank")
    val specificParams = config.readIn[Params]("trainer")
    println("Training Parser...")
    println(params)
    println(specificParams)

    val parsers = trainParser(params, specificParams)

    import params._

    for((name, parser) <- parsers) {
      println("Parser " + name)

      println("Evaluating Parser...")
      val stats = evalParser(devTrees, parser, name+"-dev")
      evalParser(testTrees, parser, name+"-test")
      import stats._
      println("Eval finished. Results:")
      println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tag Accuracy: " + tagAccuracy)
      val outDir = new File("parsers/")
      outDir.mkdirs()
      val out = new File(outDir, name +".parser")
      writeObject(out, parser)
    }
  }

  def evalParser(testTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                 parser: Parser[AnnotatedLabel, String],
                 name: String):ParseEval.Statistics = {
    ParseEval.evaluateAndLog(testTrees, parser, name, AnnotatedLabelChainReplacer, { (_: AnnotatedLabel).label })
  }

}
