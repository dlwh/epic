package scalanlp.parser

import collection.mutable.ArrayBuffer
import scalanlp.trees.{TstTreebank, Trees, Tree, BinarizedTree}

/**
 *
 * @author dlwh
 */
trait ParserTestHarness {
  def getTrainTrees(binarization:(Tree[String]=>BinarizedTree[String]) = (Trees.xBarBinarize _),
                    maxLength:Int= 10) = {
    val treebank = {
      TstTreebank.treebank;
    }
    massageTrees(treebank.trainTrees,binarization,maxLength);
  }

  def getTestTrees(binarization:(Tree[String]=>BinarizedTree[String]) = (Trees.xBarBinarize _),
                   maxLength:Int= 10) = {
    val treebank = {
      TstTreebank.treebank;
    }
    massageTrees(treebank.testTrees,binarization,maxLength);
  }

  def massageTrees(trees: Iterator[(Tree[String],Seq[String])],
                   binarize:(Tree[String]=>BinarizedTree[String]) = (Trees.xBarBinarize _),
                   maxLength:Int=10) = {
    val xform = Trees.Transforms.StandardStringTransform;
    val trainTrees = ArrayBuffer() ++= (for( (tree,words) <- trees.filter(_._2.length <= maxLength))
    yield (binarize(xform(tree)),words));

    trainTrees
  }


  def evalParser(testTrees: IndexedSeq[(Tree[String],Seq[String])],parser: Parser[String,String]) = {
    val (prec,recall,exact) = ParseEval.evaluate(testTrees,parser);
    val f1 = (2 * prec * recall)/(prec + recall);
    (prec,recall,exact,f1);
  }
}