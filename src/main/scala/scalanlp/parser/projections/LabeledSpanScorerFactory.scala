package scalanlp.parser
package projections

import scalanlp.trees._
import scalanlp.collection.mutable.TriangularArray
import scalanlp.math.Numerics;

import java.io._
import scalanlp.concurrent.ParallelOps._
import scalanlp.trees.DenseTreebank

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
class LabeledSpanScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                    indexedProjections: ProjectionIndexer[C,L]) extends SpanScorer.Factory[W] {

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer = SpanScorer.identity) = {
    val coarseRootIndex = parser.grammar.index(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);

    val sentProb = inside(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(inside,outside,sentProb);

    chartScorer
  }

  def buildSpanScorer(inside: ParseChart[L], outside: ParseChart[L], sentProb: Double):LabeledSpanScorer = {
    val scores = TriangularArray.raw(inside.length+1,inside.grammar.fillArray(Double.NegativeInfinity));
    for(begin <-  0 until inside.length; end <- begin+1 to (inside.length)) {
      for(l <- inside.enteredLabelIndexes(begin,end)) {
        val index = TriangularArray.index(begin, end)
        val pL = indexedProjections.project(l)
        val currentScore = scores(index)(pL);
        val myScore = inside.labelScore(begin, end, l) + outside.labelScore(begin, end, l) - sentProb
        scores(index)(pL) = Numerics.logSum(currentScore,myScore);
      }
    }
    return new LabeledSpanScorer(scores);
  }

}

@serializable
@SerialVersionUID(1)
class LabeledSpanScorer(scores: Array[Array[Double]]) extends SpanScorer {
  @inline
  private def score(begin: Int, end: Int, label: Int) = {
    val score =  scores(TriangularArray.index(begin,end))(label)
    score
  }

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = score(begin,end,parent);

  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    score(begin,end,parent);
  }
  def scoreLexical(begin: Int, end: Int, tag: Int): Double = {
    score(begin,end,tag);
  }
}

object ProjectTreebankToLabeledSpans {
  val TRAIN_SPANS_NAME = "train.spans.ser"
  val DEV_SPANS_NAME = "dev.spans.ser"
  val TEST_SPANS_NAME = "test.spans.ser"
  def main(args: Array[String]) {
    val parser = loadParser(new File(args(0)));
    val treebank = DenseTreebank.fromZipFile(new File(args(1)));
    val outDir = new File(args(2));
    outDir.mkdirs();
    val projections = new ProjectionIndexer(parser.builder.grammar.index,parser.builder.grammar.index,identity[String])
    val factory = new LabeledSpanScorerFactory[String,String,String](parser.builder.withCharts(ParseChart.logProb),projections);
    writeObject(mapTrees(factory,treebank.trainTrees.toIndexedSeq),new File(outDir,TRAIN_SPANS_NAME))
    writeObject(mapTrees(factory,treebank.testTrees.toIndexedSeq),new File(outDir,TEST_SPANS_NAME))
    writeObject(mapTrees(factory,treebank.devTrees.toIndexedSeq),new File(outDir,DEV_SPANS_NAME))
  }

  def loadParser(loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[ChartParser[String,String,String]]
    oin.close();
    parser;
  }

  def mapTrees(factory: SpanScorer.Factory[String], trees: IndexedSeq[(Tree[String],Seq[String])]) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { case (tree,words) =>
      println(words);
      try {
        factory.mkSpanScorer(words)
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

  def loadSpans(spanDir: File) = {
    if(!spanDir.exists || !spanDir.isDirectory) error(spanDir + " must exist and be a directory!")

    val trainSpans = loadSpansFile(new File(spanDir,TRAIN_SPANS_NAME));
    val devSpans = loadSpansFile(new File(spanDir,DEV_SPANS_NAME));
    val testSpans = loadSpansFile(new File(spanDir,TEST_SPANS_NAME));

    (trainSpans,devSpans,testSpans);
  }

  def loadSpansFile(spanFile: File) = {
    require(spanFile.exists, spanFile + " must exist!")
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(spanFile)));
    val spans = oin.readObject().asInstanceOf[IndexedSeq[SpanScorer]]
    oin.close();
    spans;
  }

}

