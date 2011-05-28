package scalanlp.parser
package projections

import scalanlp.collection.mutable.TriangularArray
import scalala.tensor.sparse.SparseVector
import scalanlp.trees.{Trees, BinarizedTree, Tree, DenseTreebank}
import scalanlp.io.FileIterable
import java.io._
import scalanlp.util.Index
import scalanlp.concurrent.ParallelOps._;


/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRuleScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                     val indexedProjections: ProjectionIndexer[C,L],
                                     pruningThreshold: Double = -7)
        extends ChartDrivenScorerFactory[C,L,W](parser,indexedProjections,pruningThreshold) {

  type MyScorer = AnchoredRuleScorer[C];
  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores,logTotalsUnaries,binaryScores,logTotals) = ruleData;
    new AnchoredRuleScorer(lexicalScores, unaryScores, binaryScores, logTotals, logTotalsUnaries);
  }

}



/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRulePosteriorScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                     indexedProjections: ProjectionIndexer[C,L],
                                     pruningThreshold: Double = -7)
        extends ChartDrivenScorerFactory[C,L,W](parser,indexedProjections,pruningThreshold) {

  type MyScorer = AnchoredRuleScorer[C];
  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val zeroSparseVector = new SparseVector(indexedProjections.coarseIndex.size, 0);
    zeroSparseVector.default = 0.0;
    val logTotals = TriangularArray.raw(ruleData.lexicalScores.length+1,zeroSparseVector);
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores, _, binaryScores, _) = ruleData;
    new AnchoredRuleScorer(lexicalScores, unaryScores, binaryScores, logTotals, logTotals);
  }

}


@serializable
@SerialVersionUID(2)
class AnchoredRuleScorer[L](lexicalScores: Array[SparseVector], // begin -> label -> score
                         // (begin,end) -> parent -> child -> score
                         unaryScores: Array[Array[SparseVector]],
                         // (begin,end) -> (split-begin) -> parent -> lchild -> rchild -> score
                         // so many arrays.
                         binaryScores: Array[Array[Array[Array[SparseVector]]]],
                         logTotalBinaries: Array[SparseVector], // sum of scores for binary scores.
                         logTotalUnaries: Array[SparseVector] // sum of scores for unary scores.
                        ) extends SpanScorer[L] {

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else if(forSpan(parent) eq null) Double.NegativeInfinity
    else if(logTotalUnaries(TriangularArray.index(begin,end)) eq null)  Double.NegativeInfinity
    else {
      val r= forSpan(parent)(child) - logTotalUnaries(TriangularArray.index(begin,end))(parent);
      if(logTotalUnaries(TriangularArray.index(begin,end))(parent) == Double.NegativeInfinity) Double.NegativeInfinity
      else r
    }
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    val forSpan = binaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else {
      val forSplit = forSpan(split - begin)
      if(forSplit eq null) Double.NegativeInfinity
      else {
        val forParent = forSplit(parent)
        if(forParent == null || forParent(leftChild) == null) Double.NegativeInfinity
        else {
          val r = forParent(leftChild)(rightChild) - logTotalBinaries(TriangularArray.index(begin,end))(parent);
          if(logTotalBinaries(TriangularArray.index(begin,end))(parent) == Double.NegativeInfinity) Double.NegativeInfinity
          else r

        }
      }
    }
  }
  def scoreLexical(begin: Int, end: Int, tag: Int): Double = {
    lexicalScores(begin)(tag);
  }
}

object ProjectTreebankToVarGrammar {
  val TRAIN_SPANS_NAME = "train.spans.ser"
  val DEV_SPANS_NAME = "dev.spans.ser"
  val TEST_SPANS_NAME = "test.spans.ser"
  val SPAN_INDEX_NAME = "spanindex.ser"
  def main(args: Array[String]) {
    val parser = loadParser(new File(args(0)));
    val coarseParser = ProjectTreebankToLabeledSpans.loadParser(new File(args(1)));
    val treebank = ProcessedTreebank(TreebankParams(new File(args(1))),SpanParams(new File(args(3))));
    val outDir = new File(args(4));
    outDir.mkdirs();
    val projections = ProjectionIndexer[String,(String,Int)](coarseParser.builder.grammar.index,parser.builder.grammar.index,_._1);
    val factory = new AnchoredRuleScorerFactory[String,(String,Int),String](parser.builder.withCharts(ParseChart.logProb),projections, -100);
    writeObject(parser.builder.grammar.index,new File(outDir,SPAN_INDEX_NAME));
    writeIterable(mapTrees(factory,treebank.trainTrees,true),new File(outDir,TRAIN_SPANS_NAME))
    writeIterable(mapTrees(factory,treebank.testTrees,false),new File(outDir,TEST_SPANS_NAME))
    writeIterable(mapTrees(factory,treebank.devTrees,false),new File(outDir,DEV_SPANS_NAME))
  }

  def loadParser(loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[ChartParser[String,(String,Int),String]]
    oin.close();
    parser;
  }


  def mapTrees(factory: AnchoredRuleScorerFactory[String,(String,Int),String], trees: IndexedSeq[TreeInstance[String,String]], useTree: Boolean) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { case TreeInstance(_,tree,words, scorer) =>
      println(words);
      try {
        val proj: ProjectingSpanScorer[String, (String, Int)] = new ProjectingSpanScorer(factory.indexedProjections,scorer,true);
        val res = factory.mkSpanScorer(words,proj)
        res;
      } catch {
        case e: Exception => e.printStackTrace(); SpanScorer.identity;
      }
    }
  }

  def writeObject(o: AnyRef, file: File) {
    val oout = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(file)));
    oout.writeObject(o);
    oout.close();
  }

  def writeIterable[T](o: Iterable[T], file: File) {
    FileIterable.write(o,file);
  }

  def loadSpans(spanDir: File) = {
    if(!spanDir.exists || !spanDir.isDirectory) error(spanDir + " must exist and be a directory!")

    val trainSpans = loadSpansFile(new File(spanDir,TRAIN_SPANS_NAME));
    val devSpans = loadSpansFile(new File(spanDir,DEV_SPANS_NAME));
    val testSpans = loadSpansFile(new File(spanDir,TEST_SPANS_NAME));

    (trainSpans,devSpans,testSpans);
  }

  def loadSpansFile(spanFile: File):Iterable[SpanScorer[String]] = {
    require(spanFile.exists, spanFile + " must exist!")
    new FileIterable[SpanScorer[String]](spanFile);
  }

  def loadSpanIndex(spanFile: File) = {
    require(spanFile.exists, spanFile + " must exist!")
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(spanFile)));
    val index = oin.readObject().asInstanceOf[Index[String]];
    oin.close();
    index;
  }

}