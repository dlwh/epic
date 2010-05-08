package scalanlp.parser;

import java.io.File
import java.io.FileInputStream
import java.util.Properties
import scala.collection.mutable.ArrayBuffer
import scalanlp.config.Configuration
import scalanlp.trees.BinarizedTree
import scalanlp.trees.DenseTreebank
import scalanlp.trees.Tree
import scalanlp.trees.Treebank
import scalanlp.trees.Trees

trait ParserTester {
  def trainParser(trainTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  devTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  config: Configuration):Iterator[(String,Parser[String,String])];

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

    val trainTrees = for( (tree,words) <- treebank.trainTrees.view.filter(_._2.length < maxLength))
      yield (binarize(xform(tree)),words);

    val devTrees = for( (tree,words) <- treebank.devTrees.view.filter(_._2.length < maxLength))
      yield (binarize(xform(tree)),words);

    println("Training Parser...");
    val parsers = trainParser(trainTrees,devTrees,config);

    for((name,parser) <- parsers) {
      println("Parser " + name);
      val testTrees = treebank.testTrees.filter(_._2.length <= maxLength);

      evalParser(testTrees,parser,xform);
    }
  }

  protected def evalParser(testTrees: Seq[(Tree[String],Seq[String])], parser: Parser[String,String], xform: Tree[String]=>Tree[String]) = {
    println("Evaluating Parser...");
    val arr = new ArrayBuffer() ++= testTrees;
    val (prec,recall,exact) = ParseEval.evaluate(arr,parser,xform);
    val f1 = (2 * prec * recall)/(prec + recall);
    println("Eval finished. Results:");
    println( "P: " + prec + " R:" + recall + " F1: " + f1 +  " Ex:" + exact);
  }

}

