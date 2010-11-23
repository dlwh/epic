package scalanlp.parser;

import java.util.Properties
import scala.collection.mutable.ArrayBuffer
import scalanlp.config.Configuration
import scalanlp.trees.BinarizedTree
import scalanlp.trees.DenseTreebank
import scalanlp.trees.Tree
import scalanlp.trees.Treebank
import scalanlp.trees.Trees
import java.io._

trait ParserTrainer {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  config: Configuration):Iterator[(String,Parser[String,String])];


  def transformTrees(trees: Iterator[(Tree[String],Seq[String])],
                     spans: Iterable[SpanScorer],
                     maxLength: Int,
                     binarize: (Tree[String]) => BinarizedTree[String],
                     xform: Tree[String]=>Tree[String]): IndexedSeq[(BinarizedTree[String], Seq[String],SpanScorer)] = {

    val transformed =  for {
      ((tree, words),span) <- (trees zip spans.iterator) if words.length <= maxLength
    } yield {
      (binarize(xform(tree)), words,span)
    };

    ArrayBuffer() ++= transformed;
  }

  def loadTrainSpans(config: Configuration):Iterable[SpanScorer] = Stream.continually(SpanScorer.identity);
  def loadTestSpans(config: Configuration):Iterable[SpanScorer] = Stream.continually(SpanScorer.identity);
  def loadDevSpans(config: Configuration):Iterable[SpanScorer] = Stream.continually(SpanScorer.identity);

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
    val trainTrees = transformTrees(treebank.trainTrees, trainSpans, maxLength, binarize, xform);

    val devSpans = loadDevSpans(config);
    val devTrees = transformTrees(treebank.devTrees, devSpans, maxLength, binarize, xform)

    println("Training Parser...");
    val parsers = trainParser(trainTrees,devTrees,config);
    val testSpans = loadTestSpans(config);
    val testTrees = transformTrees(treebank.testTrees, testSpans, maxLength, binarize, xform)

    for((name,parser) <- parsers) {
      println("Parser " + name);

      evalParser(testTrees,parser,name);
      val outDir = new File("parsers/");
      outDir.mkdirs();
      val out = new File(outDir,name + ".parser");
      val stream = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(out)));
      stream.writeObject(parser);
      stream.close();
    }
  }

  protected def evalParser(testTrees: IndexedSeq[(Tree[String],Seq[String],SpanScorer)], parser: Parser[String,String], name: String) = {
    println("Evaluating Parser...");
    val (prec,recall,exact) = ParseEval.evaluateAndLog(testTrees,parser,name);
    val f1 = (2 * prec * recall)/(prec + recall);
    println("Eval finished. Results:");
    println( "P: " + prec + " R:" + recall + " F1: " + f1 +  " Ex:" + exact);
    f1
  }

}

