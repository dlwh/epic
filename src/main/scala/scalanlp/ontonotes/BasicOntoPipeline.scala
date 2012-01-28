package scalanlp.ontonotes

import scalanlp.config.Configuration
import java.io.File
import scalanlp.parser.TreeInstance._
import scalanlp.parser._
import scalanlp.trees.{Tree, UnaryChainRemover, Trees}
import scalala.library.Library
import scalanlp.util._
import scalanlp.parser.ParseEval.Statistics

/**
 * 
 * @author dlwh
 */

trait BasicOntoPipeline {

  /**
   * The type of the parameters to read in via scalanlp.config
   */
  type Params;
  /**
   * Required manifest for the params
   */
  protected implicit val paramManifest: Manifest[Params];
  def stripAnnotations(t: OntoLabel) = OntoLabel(t.tag)

  def trainParser(trainTrees: IndexedSeq[TreeInstance[OntoLabel,String]],
                  validate: Parser[OntoLabel,String]=>ParseEval.Statistics,
                  params: Params):Iterator[(String,Parser[OntoLabel,String])];

  def main(args: Array[String]) {

    val (baseConfig,files) = scalanlp.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[ProcessedOntobank]("corpus")
    val specificParams = config.readIn[Params]("trainer");

    println("Training Parser...");
    val trainTrees = for( ti <- params.trainTrees) yield ti
    val devTrees = for( ti <- params.devTrees) yield ti.copy(ti.id, ti.tree.map(stripAnnotations _))
    val testTrees = params.testTrees.map(ti => ti.copy(tree=ti.tree.map(stripAnnotations _)))



    val validateTrees = devTrees.take(400)
    def validate(parser: Parser[OntoLabel,String]) = {
      ParseEval.evaluate[OntoLabel](validateTrees, parser, params.replacer, asString = {(_:OntoLabel).tag})
    }

    for((name,parser) <- trainParser(trainTrees,validate _, specificParams)) {
      println("Parser " + name);

      println("Evaluating Parser...");
      val stats= ParseEval.evaluateAndLog(testTrees, parser:Parser[OntoLabel, String], "Onto", params.replacer, {(_:OntoLabel).tag})
      import stats._;
      println("Eval finished. Results:");
      println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tag Accuracy: " + tagAccuracy);
      val outDir = new File("parsers/");
      outDir.mkdirs();
      val out = new File(outDir,name +".parser")
      writeObject(out,parser);
    }
  }
}

object GenerativeOntoPipeline extends BasicOntoPipeline {
  type Params = ParserParams.Params;
  protected val paramManifest = manifest[Params];

  def trainParser(trainTrees: IndexedSeq[TreeInstance[OntoLabel, String]], validate: (Parser[OntoLabel, String]) => Statistics, params: Params) = {
    val trainTrees2 = for( ti <- trainTrees) yield ti.copy(ti.id, ti.tree.map(stripAnnotations _))
    val (words,binary,unary) = GenerativeParser.extractCounts(trainTrees2)
    val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary))
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
    val parser = SimpleChartParser(CKYChartBuilder(OntoLabel("TOP"),lexicon,grammar).withCharts(ParseChart.logProb));
    Iterator.single("Onto" -> parser)
  }

}

/**
 * Represents a treebank with attendant spans, binarization, etc. Used in all the parser trainers.
 *
 * @author dlwh
 */
case class ProcessedOntobank(treebank: TreebankParams) {
  import treebank.maxLength

  val corpus = Corpus.fromXMLDirectory(treebank.path)
  lazy val trainTreesWithUnaries = transformTrees(corpus.train)
  lazy val (trainTrees,replacer) = removeUnaryChains(trainTreesWithUnaries);

  lazy val devTrees = transformTrees(corpus.dev)

  lazy val testTrees = transformTrees(corpus.test)

  implicit val lens = new Lens[OntoLabel, String] {
    def get(t: OntoLabel) = t.tag

    def set(t: OntoLabel, u: String) = t.copy(u)
  }

  def transformTrees(portion: corpus.Portion): IndexedSeq[TreeInstance[OntoLabel,String]] = {
    val transform = new Trees.Transforms.LensedStandardTransform[OntoLabel]
    val binarizedAndTransformed = for (
      ((tree, words),index) <- portion.trees.zipWithIndex if words.length <= maxLength
    ) yield {
      val transformed = transform(tree)
      def relabel(l: OntoLabel, u: OntoLabel) = if (l.tag.startsWith("@")) l else l.copy(tag = "@" + l.tag)
      val bin = Trees.binarize(transformed,relabel _)
      val name = portion.name +"-" + index
      TreeInstance[OntoLabel,String](name,bin,words,SpanScorer.identity)
    }

    binarizedAndTransformed.toIndexedSeq
  }

  def stripAnnotations(t: OntoLabel) = OntoLabel(t.tag)


  // TODO: this needs a lens
  def removeUnaryChains(trees: IndexedSeq[TreeInstance[OntoLabel,String]]) = {
    val chainRemover = new UnaryChainRemover[OntoLabel](stripAnnotations _);

    val (dechained,chainReplacer) = chainRemover.removeUnaryChains(trees.iterator.map { ti => (ti.tree,ti.words)})

    val dechainedWithSpans = for {
      ((t,w),TreeInstance(id,_,_,span)) <- (dechained zip trees)
    } yield TreeInstance(id,t,w,span);

    (dechainedWithSpans, chainReplacer)
  }

}
