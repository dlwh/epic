package scalanlp.parser;

import java.io.File
import java.io.FileInputStream
import java.util.Properties
import scalanlp.config.Configuration
import scalanlp.trees.BinarizedTree
import scalanlp.trees.DenseTreebank
import scalanlp.trees.Tree
import scalanlp.trees.Treebank
import scalanlp.trees.Trees

trait ParserTester {
  def trainParser(trainTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  devTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  config: Configuration):Parser[String,String];

  def main(args: Array[String]) {
    val properties = new Properties();
    properties.load(new FileInputStream(new File(args(0))));
    val config = Configuration.fromProperties(properties);

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

    val trainTrees = for( (tree,words) <- treebank.trainTrees.view)
      yield (binarize(xform(tree)),words);

    val devTrees = for( (tree,words) <- treebank.devTrees.view)
      yield (binarize(xform(tree)),words);

    println("Training Parser...");
    val parser = trainParser(trainTrees,devTrees,config);


    println("Evaluating Parser...");
    val testTrees = treebank.testTrees.iterator.filter(_._2.length <= maxLength);
    val (prec,recall,exact) = ParseEval.evaluate(testTrees,parser,xform);
    val f1 = (2 * prec * recall)/(prec + recall);
    println("Eval finished. Results:");
    println( "P: " + prec + " R:" + recall + " F1: " + f1 +  " Ex:" + exact);
  }

}

