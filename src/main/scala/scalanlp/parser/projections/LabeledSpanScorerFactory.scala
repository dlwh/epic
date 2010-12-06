package scalanlp.parser
package projections

import scalanlp.trees._
import scalanlp.collection.mutable.TriangularArray
import scalanlp.math.Numerics
import scalanlp.util.Index
import scalala.tensor.sparse.SparseVector
import scalanlp.io.FileIterable;

import java.io._
import scalanlp.concurrent.ParallelOps._
import scalanlp.trees.DenseTreebank

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
class LabeledSpanScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                    indexedProjections: ProjectionIndexer[C,L],
                                    pruningThreshold: Double= -10) extends SpanScorer.Factory[W] {

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
    val scores = TriangularArray.raw(inside.length+1,null:SparseVector);
    var density = 0;
    var labelDensity = 0;
    for(begin <-  0 until inside.length; end <- begin+1 to (inside.length)) {
      val index = TriangularArray.index(begin, end)
      for(l <- inside.enteredLabelIndexes(begin,end)) {
        val pL = indexedProjections.project(l)
        val myScore = inside.labelScore(begin, end, l) + outside.labelScore(begin, end, l) - sentProb
        if(myScore >= pruningThreshold) {
          if(scores(index) == null) {
            scores(index) = indexedProjections.coarseEncoder.mkSparseVector(Double.NegativeInfinity);
            density += 1;
          }
          val currentScore = scores(index)(pL);
          if(currentScore == Double.NegativeInfinity) labelDensity += 1;
          scores(index)(pL) = Numerics.logSum(currentScore,myScore);
        }
      }
    }
    println("Density: " + density * 1.0 / scores.length);
    println("Label Density:" + labelDensity * 1.0 / scores.length / parser.grammar.index.size)
    new LabeledSpanScorer(scores);
  }

}

@serializable
@SerialVersionUID(1)
class LabeledSpanScorer(scores: Array[SparseVector]) extends SpanScorer {
  @inline
  private def score(begin: Int, end: Int, label: Int) = {
    if(scores(TriangularArray.index(begin,end)) eq null) Double.NegativeInfinity
    else scores(TriangularArray.index(begin,end))(label)
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
  val SPAN_INDEX_NAME = "spanindex.ser"
  def main(args: Array[String]) {
    val parser = loadParser(new File(args(0)));
    val treebank = DenseTreebank.fromZipFile(new File(args(1)));
    val outDir = new File(args(2));
    outDir.mkdirs();
    val projections = new ProjectionIndexer(parser.builder.grammar.index,parser.builder.grammar.index,identity[String])
    val factory = new LabeledSpanScorerFactory[String,String,String](parser.builder.withCharts(ParseChart.logProb),projections);
    writeObject(parser.builder.grammar.index,new File(outDir,SPAN_INDEX_NAME));
    writeIterable(mapTrees(factory,treebank.trainTrees.toIndexedSeq),new File(outDir,TRAIN_SPANS_NAME))
    writeIterable(mapTrees(factory,treebank.testTrees.toIndexedSeq),new File(outDir,TEST_SPANS_NAME))
    writeIterable(mapTrees(factory,treebank.devTrees.toIndexedSeq),new File(outDir,DEV_SPANS_NAME))
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
        val scorer = factory.mkSpanScorer(words)
        scorer;
      } catch {
        case e: Exception => e.printStackTrace(); error("rawr");
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

  def loadSpansFile(spanFile: File):Iterable[SpanScorer] = {
    require(spanFile.exists, spanFile + " must exist!")
    new FileIterable[SpanScorer](spanFile);
  }

  def loadSpanIndex(spanFile: File) = {
    require(spanFile.exists, spanFile + " must exist!")
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(spanFile)));
    val index = oin.readObject().asInstanceOf[Index[String]];
    oin.close();
    index;
  }

}

object ProjectTreebankToLabeledSpansProjective {
  import ProjectTreebankToLabeledSpans._;


  def mapTrees(factory: SpanScorer.Factory[String], trees: IndexedSeq[((Tree[String],Seq[String]),SpanScorer)]) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { case ((tree,words),oldScorer) =>
      println(words);
      try {
        val scorer = factory.mkSpanScorer(words,oldScorer)
        scorer;
      } catch {
        case e: Exception => e.printStackTrace(); error("rawr");
      }
    }
  }

  def main(args: Array[String]) {
    val genParser = ProjectTreebankToLabeledSpans.loadParser(new File(args(0)));
    val latentParser = loadParser(new File(args(1)));
    val treebank = DenseTreebank.fromZipFile(new File(args(2)));
    val outDir = new File(args(3));
    val inSpanDir = new File(args(4));
    def unsplit(x: (String,Int)) = x._1
    val projections = new ProjectionIndexer(genParser.builder.grammar.index,latentParser.builder.grammar.index,unsplit _)

    def projectScorer(coarseScorer: SpanScorer) = new ProjectingSpanScorer(projections, coarseScorer);

    val trainSpans = loadSpansFile(new File(inSpanDir,TRAIN_SPANS_NAME)).map(projectScorer _);
    val testSpans = loadSpansFile(new File(inSpanDir,TEST_SPANS_NAME)).map(projectScorer _);
    val devSpans = loadSpansFile(new File(inSpanDir,DEV_SPANS_NAME)).map(projectScorer _);

    outDir.mkdirs();
    val factory = new LabeledSpanScorerFactory[String,(String,Int),String](latentParser.builder.withCharts(ParseChart.logProb),projections);

    writeObject(genParser.builder.grammar.index,new File(outDir,SPAN_INDEX_NAME));
    writeIterable(mapTrees(factory,treebank.trainTrees.toIndexedSeq zip trainSpans),new File(outDir,TRAIN_SPANS_NAME))
    writeIterable(mapTrees(factory,treebank.testTrees.toIndexedSeq zip testSpans),new File(outDir,TEST_SPANS_NAME))
    writeIterable(mapTrees(factory,treebank.devTrees.toIndexedSeq zip devSpans),new File(outDir,DEV_SPANS_NAME))
  }

  def loadParser(loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[ChartParser[(String,Int),String,String]]
    oin.close();
    parser;
  }

}