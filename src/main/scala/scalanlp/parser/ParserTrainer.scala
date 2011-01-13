package scalanlp.parser;

import java.util.Properties
import projections.ProjectTreebankToLabeledSpans
import scala.collection.mutable.ArrayBuffer
import scalanlp.config._
import java.io._
import scalanlp.trees._
import scalanlp.util._
import scalanlp.trees.UnaryChainRemover.ChainReplacer

case class TreebankParams(path: File, maxLength:Int = 40, binarization:String = "xbar", processing: String = "standard") {
  def binarize = {
    if(binarization == "xbar") Trees.xBarBinarize _ ;
    else Trees.binarize(_:Tree[String]);
  }

  val treebank = {
    if(path.isDirectory) Treebank.fromPennTreebankDir(path)
    else DenseTreebank.fromZipFile(path);
  }

  val xform = processing match {
    case "german" => Trees.Transforms.GermanTreebankTransform;
    case _ => Trees.Transforms.StandardStringTransform;
  }

}

case class Spans(directory: File = null) {
  def loadSpans(path: Option[File]):Iterable[SpanScorer[String]] = path match {
    case Some(path) => ProjectTreebankToLabeledSpans.loadSpansFile(path);
    case None =>       Stream.continually(SpanScorer.identity)
  }

  val trainSpansFile = Option(directory).map{dir => new File(dir, ProjectTreebankToLabeledSpans.TRAIN_SPANS_NAME)}
  val trainSpans = loadSpans(trainSpansFile);

  val devSpansFile = Option(directory).map{dir => new File(dir, ProjectTreebankToLabeledSpans.DEV_SPANS_NAME)}
  val devSpans = loadSpans(devSpansFile);

  val testSpansFile = Option(directory).map{dir => new File(dir, ProjectTreebankToLabeledSpans.TEST_SPANS_NAME)}
  val testSpans = loadSpans(testSpansFile);
}

case class ParserTrainerParams(treebank: TreebankParams, spans: Spans) {

}

trait ParserTrainer {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer[String])],
                  config: Configuration):Iterator[(String,Parser[String,String])];


  def transformTrees(trees: Iterator[(Tree[String],Seq[String])],
                     spans: Iterable[SpanScorer[String]],
                     maxLength: Int,
                     binarize: (Tree[String]) => BinarizedTree[String],
                     xform: Tree[String]=>Tree[String]): IndexedSeq[(BinarizedTree[String], Seq[String],SpanScorer[String])] = {

    val binarizedAndTransformed = for {
      ((tree, words),span) <- (trees zip spans.iterator) if words.length <= maxLength
    } yield (binarize(xform(tree)),words,span)

      binarizedAndTransformed.toIndexedSeq

  }

  def removeUnaryChains(trees: IndexedSeq[(BinarizedTree[String],Seq[String],SpanScorer[String])]) = {
    val chainRemover = new UnaryChainRemover[String];

    val (dechained,chainReplacer) = chainRemover.removeUnaryChains(trees.iterator.map { case (t,w,s) => (t,w) });

    val dechainedWithSpans = for {
      ((t,w),(_,_,span)) <- (dechained zip trees)
    } yield (t,w,span);

    (dechainedWithSpans, chainReplacer)
  }

  var unaryReplacer : ChainReplacer[String] = _;

  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map{new File(_)});
    val params = config.readIn[ParserTrainerParams]("treebank");
    println(params);
    import params.treebank._;
    import params.spans._;


    val trainTreesWithUnaries = transformTrees(treebank.trainTrees, trainSpans, maxLength, binarize, xform);
    val (trainTrees,replacer) = removeUnaryChains(trainTreesWithUnaries);
    unaryReplacer = replacer;

    val devTrees = transformTrees(treebank.devTrees, devSpans, maxLength, binarize, xform)

    println("Training Parser...");
    val parsers = trainParser(trainTrees,devTrees,config);

    val testTrees = transformTrees(treebank.testTrees, testSpans, maxLength, binarize, xform)

    for((name,parser) <- parsers) {
      println("Parser " + name);

      evalParser(testTrees,parser,name,replacer);
      val outDir = new File("parsers/");
      outDir.mkdirs();
      val out = new File(outDir,name +".parser")
      writeObject(out,parser);
    }
  }

  protected def evalParser(testTrees: IndexedSeq[(Tree[String],Seq[String],SpanScorer[String])],
          parser: Parser[String,String], name: String, chainReplacer: ChainReplacer[String]) = {
    println("Evaluating Parser...");
    val (prec,recall,exact) = ParseEval.evaluateAndLog(testTrees,parser,name,chainReplacer);
    val f1 = (2 * prec * recall)/(prec + recall);
    println("Eval finished. Results:");
    println( "P: " + prec + " R:" + recall + " F1: " + f1 +  " Ex:" + exact);
    f1
  }

}

