package scalanlp.parser
package projections

import scalanlp.trees._;

import java.io._
import scalanlp.concurrent.ParallelOps._
import scalanlp.trees.DenseTreebank

/**
 * Creates labeled span scorers for a set of trees from some parser. Does not do any projection.
 * @author dlwh
 */
class LabeledSpanScorerFactory[L,W](parser: ChartParser[ParseChart.LogProbabilityParseChart,L,W]) extends SpanScorer.Factory[W] {

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer = ChartParser.defaultScorer) = {
    val coarseRootIndex = parser.grammar.index(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);

    val sentProb = inside(0,s.length,coarseRootIndex);
    assert(!sentProb.isInfinite, s);

    val chartScorer = buildSpanScorer(inside,outside,sentProb);

    chartScorer
  }

  def buildSpanScorer(inside: ParseChart[L], outside: ParseChart[L], sentProb: Double):SpanScorer = new SpanScorer {
    @inline
    def score(begin: Int, end: Int, label: Int) = {
      val score =  (inside(begin,end,label)
              + outside(begin,end,label) - sentProb);
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

}

object ProjectTreebankToLabeledSpans {
  def main(args: Array[String]) {
    val parser = loadParser(new File(args(0)));
    val treebank = DenseTreebank.fromZipFile(new File(args(1)));
    val outDir = new File(args(2));
    outDir.mkdirs();
    val factory = new LabeledSpanScorerFactory[String,String](parser);
    writeObject(mapTrees(factory,treebank.trainTrees.toIndexedSeq),new File(outDir,"train.spans.ser"))
    writeObject(mapTrees(factory,treebank.testTrees.toIndexedSeq),new File(outDir,"test.spans.ser"))
    writeObject(mapTrees(factory,treebank.devTrees.toIndexedSeq),new File(outDir,"dev.spans.ser"))
  }

  def loadParser(loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[ChartParser[ParseChart.LogProbabilityParseChart,String,String]]
    oin.close();
    parser;
  }

  def mapTrees(factory: SpanScorer.Factory[String], trees: IndexedSeq[(Tree[String],Seq[String])]) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { case (tree,words) => factory.mkSpanScorer(words) }
  }

  def writeObject(o: AnyRef, file: File) {
    val oout = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(file)));
    oout.writeObject(o);
    oout.close();
  }
}
