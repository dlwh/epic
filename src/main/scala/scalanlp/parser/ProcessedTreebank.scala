package scalanlp.parser

import projections._
import scalanlp.trees._;
import java.io.File
import scalanlp.data.Example
import scalanlp.util.Index

/**
 * Represents a treebank with attendant spans, binarization, etc. Used in all the parser trainers.
 *
 * @author dlwh
 */
case class ProcessedTreebank(path: File,
                             maxLength:Int = 40,
                             binarization:String = "head") {

  lazy val treebank = {
    if(path.isDirectory) Treebank.fromPennTreebankDir(path)
    else DenseTreebank.fromZipFile(path);
  }

  lazy val trainTreesWithUnaries = transformTrees(treebank.train, maxLength);
  lazy val trainTrees = trainTreesWithUnaries.map(ti => ti.copy(tree=UnaryChainRemover.removeUnaryChains(ti.tree)))
  lazy val devTrees = transformTrees(treebank.dev, 100000);
  lazy val testTrees = transformTrees(treebank.test, 1000000);


  def transformTrees(portion: treebank.Portion, maxL: Int): IndexedSeq[TreeInstance[AnnotatedLabel, String]] = {
    val binarizedAndTransformed = for (
      ((tree, words), index) <- portion.trees.zipWithIndex if words.length <= maxL
    ) yield {
      val transformed = process(tree)
      val name = portion.name +"-" + index
      TreeInstance(name, transformed, words)
    }

    binarizedAndTransformed.toIndexedSeq
  }

  def headRules = {
    binarization match {
      case "xbar" | "right" => HeadFinder.right[String]
      case "leftXbar" | "left" => HeadFinder.left[String]
      case "head" => HeadFinder.collins
      case _ => HeadFinder.collins
    }
  }

  val process = new StandardTreeProcessor(headRules)
}

case class TreeInstance[L, +W](id: String,
                               tree: BinarizedTree[L],
                               words: Seq[W]) extends Example[Tree[L], Seq[W]] {
  def mapLabels[U](f: L=>U) = copy(tree=tree.map(f))

  def label = tree;
  def features = words
}

