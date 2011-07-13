package scalanlp.parser
package projections

import scalanlp.trees._
import scalanlp.collection.mutable.TriangularArray
import scalala.library.Numerics._
import scalanlp.util.Index
import scalanlp.io.FileIterable;

import java.io._
import scalanlp.trees.{Trees,DenseTreebank}
import scalanlp.tensor.sparse.OldSparseVector
import scalala.library.Numerics

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
class LabeledSpanScorerFactory[C,L,W](parser: ChartBuilder[ParseChart,L,W],
                                    indexedProjections: ProjectionIndexer[C,L],
                                    pruningThreshold: Double= -5) extends SpanScorer.Factory[C,L,W] {

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity) = {
    val coarseRootIndex = parser.grammar.labelIndex(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);

    val sentProb = inside.top(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(inside,outside,sentProb);

    chartScorer
  }

  def mkSpanScorerWithTree(tree: BinarizedTree[C], s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity) = {
    val coarseRootIndex = parser.grammar.labelIndex(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);
    val lexicon = parser.lexicon;

    val sentProb = inside.top.labelScore(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    try {
      val chartScorer = buildSpanScorer(inside,outside,sentProb, tree);
      chartScorer
    } catch {
      case e:BlarghException =>
        println(lexicon.tagScores(s(e.w)),e.l);
       assert(false)
    }

  }

  case class BlarghException(l :L , w: Int) extends Exception;

  def goldLabels(length: Int, tree: BinarizedTree[C]) = {
    val result = TriangularArray.raw(length+1,collection.mutable.BitSet());
    if(tree != null) {
      for( t <- tree.allChildren if !t.isInstanceOf[UnaryTree[L]]) {
        try {
          result(TriangularArray.index(t.span.start,t.span.end)).+=(indexedProjections.coarseIndex(t.label));
        } catch {
          case e => println("wtf: " + t.label,e);
        }
      }
    }
    result;
  }

  def buildSpanScorer(inside: ParseChart[L], outside: ParseChart[L], sentProb: Double, tree: BinarizedTree[C] = null):LabeledSpanScorer[C] = {
    val markedSpans = goldLabels(inside.length, tree)

    val scores = TriangularArray.raw(inside.length+1,null:OldSparseVector);
    for(begin <-  0 until inside.length; end <- begin+1 to (inside.length)) {
      val index = TriangularArray.index(begin, end)
      val scoresForLocation = indexedProjections.coarseEncoder.mkOldSparseVector(Double.NegativeInfinity);
      for(l <- inside.bot.enteredLabelIndexes(begin,end)) {
        val pL = indexedProjections.project(l)
        val myScore = inside.bot.labelScore(begin, end, l) + outside.bot.labelScore(begin, end, l) - sentProb
        val currentScore = scoresForLocation(pL);
        scoresForLocation(pL) = Numerics.logSum(currentScore,myScore);
      }

      for( (c,v) <- scoresForLocation.nonzero.pairs) {
        if(v > pruningThreshold || markedSpans(index)(c)) {
          if(scores(index) eq null) {
            scores(index) = indexedProjections.coarseEncoder.mkOldSparseVector(Double.NegativeInfinity);
          }
          scores(index)(c) = v;
        }
      }

      for(c <- markedSpans(index)) {
        if(scores(index) == null || scores(index)(c) == Double.NegativeInfinity) {
          println("grrr....");
          println(parser.grammar.labelIndex.get(c) + " " + begin + " " + end + tree + " " + inside.bot.labelScore(begin,end,c) + " " + outside.bot.labelScore(begin,end,c) + " " + sentProb)
          //if(begin + 1 != end) sys.error("crashy");
        }
      }
    }
    //println("Density: " + density * 1.0 / scores.length);
    //println("Label Density:" + labelDensity * 1.0 / scores.length / parser.grammar.index.size)
    new LabeledSpanScorer[C](scores);
  }

}

@SerialVersionUID(1)
class LabeledSpanScorer[L](scores: Array[OldSparseVector]) extends SpanScorer[L] with Serializable {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

  def scoreSpan(begin: Int, end: Int, label: Int): Double = {
    if(scores(TriangularArray.index(begin,end)) eq null) Double.NegativeInfinity
    else scores(TriangularArray.index(begin,end))(label)
  }
}

object ProjectTreebankToLabeledSpans {
  val TRAIN_SPANS_NAME = "train.spans.ser"
  val DEV_SPANS_NAME = "dev.spans.ser"
  val TEST_SPANS_NAME = "test.spans.ser"
  val SPAN_INDEX_NAME = "spanindex.ser"
  def main(args: Array[String]) {
    val parser = loadParser(new File(args(0)));
    val treebank = ProcessedTreebank(TreebankParams(new File(args(1)),maxLength=10000),SpanParams());

    val outDir = new File(args(2));
    outDir.mkdirs();
    val projections = ProjectionIndexer(parser.builder.grammar.labelIndex,parser.builder.grammar.labelIndex,identity[String])
    val factory = new LabeledSpanScorerFactory[String,String,String](parser.builder.withCharts(ParseChart.viterbi),projections);
    writeObject(parser.builder.grammar.labelIndex,new File(outDir,SPAN_INDEX_NAME));
    writeIterable(mapTrees(factory,treebank.trainTrees,true),new File(outDir,TRAIN_SPANS_NAME))
    writeIterable(mapTrees(factory,treebank.testTrees,false),new File(outDir,TEST_SPANS_NAME))
    writeIterable(mapTrees(factory,treebank.devTrees,false),new File(outDir,DEV_SPANS_NAME))
  }

  def loadParser(loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[ChartParser[String,String,String]]
    oin.close();
    parser;
  }


  def mapTrees(factory: LabeledSpanScorerFactory[String,String,String], trees: IndexedSeq[TreeInstance[String,String]], useTree: Boolean) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { (ti:TreeInstance[String,String]) =>
      val TreeInstance(id,tree,words,_) = ti
      println(id,words);
      try {
        val scorer = if(useTree) factory.mkSpanScorerWithTree(tree,words) else factory.mkSpanScorer(words);
        scorer;
      } catch {
        case e: Exception => e.printStackTrace(); SpanScorer.identity;
      }
    }.seq
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
    if(!spanDir.exists || !spanDir.isDirectory) sys.error(spanDir + " must exist and be a directory!")

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
