package scalanlp.parser;

import java.util.Properties
import scala.collection.mutable.ArrayBuffer
import scalanlp.config.Configuration
import java.io._
import scalanlp.trees._
import scalanlp.trees.UnaryChainRemover.ChainReplacer

trait ParserTrainer {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  config: Configuration):Iterator[(String,Parser[String,String])];


  def transformTrees(trees: Iterator[(Tree[String],Seq[String])],
                     spans: Iterable[SpanScorer],
                     maxLength: Int,
                     binarize: (Tree[String]) => BinarizedTree[String],
                     xform: Tree[String]=>Tree[String]): (IndexedSeq[(BinarizedTree[String], Seq[String],SpanScorer)],ChainReplacer[String]) = {

    val binarizedAndTransformed = (for {
      ((tree, words),span) <- (trees zip spans.iterator) if words.length <= maxLength
    } yield ((binarize(xform(tree)),words),span)).toIndexedSeq

    val chainRemover = new UnaryChainRemover[String];

    val (dechained,chainReplacer) = chainRemover.removeUnaryChains(binarizedAndTransformed.iterator.map(_._1));

    val dechainedWithSpans = for {
      ((t,w),(_,span)) <- (dechained zip binarizedAndTransformed)
    } yield (t,w,span);

    (dechainedWithSpans, chainReplacer)
  }

  def loadTrainSpans(config: Configuration):Iterable[SpanScorer] = Stream.continually(SpanScorer.identity);
  def loadTestSpans(config: Configuration):Iterable[SpanScorer] = Stream.continually(SpanScorer.identity);
  def loadDevSpans(config: Configuration):Iterable[SpanScorer] = Stream.continually(SpanScorer.identity);

  var unaryReplacer : ChainReplacer[String] = _;

  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map{new File(_)});

    val treebank = {
      val path = config.readIn[File]("treebank.path");
      if(path.isDirectory) Treebank.fromPennTreebankDir(path)
      else DenseTreebank.fromZipFile(path);
    }

    val binarize = {
      val kind = config.readIn[String]("tree.binarization","standard");
      if(kind == "xbar") Trees.xBarBinarize _ ;
      else Trees.binarize(_:Tree[String]);
    }


    val maxLength = config.readIn[Int]("sentence.maxLength",40);

    val xform = Trees.Transforms.StandardStringTransform;

    val trainSpans = loadTrainSpans(config);
    val (trainTrees,replacer) = transformTrees(treebank.trainTrees, trainSpans, maxLength, binarize, xform);

    unaryReplacer = replacer;

    val devSpans = loadDevSpans(config);
    val devTrees = transformTrees(treebank.devTrees, devSpans, maxLength, binarize, xform)._1

    println("Training Parser...");
    val parsers = trainParser(trainTrees,devTrees,config);
    val testSpans = loadTestSpans(config);
    val testTrees = transformTrees(treebank.testTrees, testSpans, maxLength, binarize, xform)._1

    for((name,parser) <- parsers) {
      println("Parser " + name);

      evalParser(testTrees,parser,name,replacer);
      val outDir = new File("parsers/");
      outDir.mkdirs();
      val out = new File(outDir,name + ".parser");
      val stream = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(out)));
      stream.writeObject(parser);
      stream.close();
    }
  }

  protected def evalParser(testTrees: IndexedSeq[(Tree[String],Seq[String],SpanScorer)],
          parser: Parser[String,String], name: String, chainReplacer: ChainReplacer[String]) = {
    println("Evaluating Parser...");
    val (prec,recall,exact) = ParseEval.evaluateAndLog(testTrees,parser,name,chainReplacer);
    val f1 = (2 * prec * recall)/(prec + recall);
    println("Eval finished. Results:");
    println( "P: " + prec + " R:" + recall + " F1: " + f1 +  " Ex:" + exact);
    f1
  }

}

