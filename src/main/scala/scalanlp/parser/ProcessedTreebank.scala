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
case class ProcessedTreebank(treebank: TreebankParams, spans: SpanParams[AnnotatedLabel]) {
  import spans._;

  lazy val trainTreesWithUnaries = transformTrees(treebank.treebank.train, trainSpans, treebank.maxLength);
  lazy val trainTrees = trainTreesWithUnaries.map(ti => ti.copy(tree=UnaryChainRemover.removeUnaryChains(ti.tree)))

  lazy val devTrees = transformTrees(treebank.treebank.dev, devSpans, 60);

  lazy val testTrees = transformTrees(treebank.treebank.test, testSpans, 1000000);


  def transformTrees(portion: treebank.treebank.Portion,
                     broker: SpanBroker[AnnotatedLabel], maxL: Int): IndexedSeq[TreeInstance[AnnotatedLabel,String]] = {
    import treebank._;

    val binarizedAndTransformed = for (
      ((tree, words),index) <- portion.trees.zipWithIndex if words.length <= maxL
    ) yield {
      val transformed = process(tree)
      val name = portion.name +"-" + index
      val span = broker.spanForId(name)
      TreeInstance(name,transformed,words,span)
    }

    binarizedAndTransformed.toIndexedSeq
  }

}

case class TreebankParams(path: File,
                          maxLength:Int = 40,
                          binarization:String = "head") {
  def headRules = {
    binarization match {
      case "xbar" | "right" => HeadFinder.right[String]
      case "leftXbar" | "left" => HeadFinder.left[String]
      case "head" => HeadFinder.collins
      case _ => HeadFinder.collins
    }
  }

  val process = new StandardTreeProcessor(headRules)

  lazy val treebank = {
    if(path.isDirectory) Treebank.fromPennTreebankDir(path)
    else DenseTreebank.fromZipFile(path);
  }


}

case class SpanParams[T](directory: File = null) {
  def loadSpans(path: Option[File]):SpanBroker[T] = path match {
    case Some(path) =>  SpanBroker.load(path)
    case None => SpanBroker.zeroBroker[T]
  }
  import SpanBroker._

  val trainSpansFile = Option(directory).map{dir => new File(dir, TRAIN_SPANS_NAME)}
  lazy val trainSpans = loadSpans(trainSpansFile);

  val devSpansFile = Option(directory).map{dir => new File(dir, DEV_SPANS_NAME)}
  lazy val devSpans = loadSpans(devSpansFile);

  val testSpansFile = Option(directory).map{dir => new File(dir,TEST_SPANS_NAME)}
  lazy val testSpans = loadSpans(testSpansFile);

  def index = Option(directory).map { dir => SpanBroker.loadSpanIndex(new File(directory,SPAN_INDEX_NAME))}
}

case class TreeInstance[L,+W](id: String,
                             tree: BinarizedTree[L],
                             words: Seq[W],
                             spanScorer: SpanScorer[L] = SpanScorer.identity[L]) extends Example[Tree[L],(Seq[W],SpanScorer[L])] {
  def mapLabels(f: L=>L) = copy(tree=tree.map(f))

  def label = tree;
  def features = words -> spanScorer;
}

