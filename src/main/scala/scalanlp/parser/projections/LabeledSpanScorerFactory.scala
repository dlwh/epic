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
                                    pruningThreshold: Double= -5) extends SpanScorer.Factory[W] {

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer = SpanScorer.identity) = {
    val coarseRootIndex = parser.grammar.index(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);

    val sentProb = inside.top(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(inside,outside,sentProb);

    chartScorer
  }

  def mkSpanScorerWithTree(tree: BinarizedTree[C], s: Seq[W], scorer: SpanScorer= SpanScorer.identity) = {
    val coarseRootIndex = parser.grammar.index(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);
    val lexicon = parser.lexicon;

    val sentProb = inside.top.labelScore(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      error("Couldn't parse " + s + " " + sentProb)
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
    val result = TriangularArray.raw(length,collection.mutable.BitSet());
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

  def buildSpanScorer(inside: ParseChart[L], outside: ParseChart[L], sentProb: Double, tree: BinarizedTree[C] = null):LabeledSpanScorer = {
    val markedSpans = goldLabels(inside.length, tree)

    val scores = TriangularArray.raw(inside.length+1,null:SparseVector);
    for(begin <-  0 until inside.length; end <- begin+1 to (inside.length)) {
      val index = TriangularArray.index(begin, end)
      val scoresForLocation = indexedProjections.coarseEncoder.mkSparseVector(Double.NegativeInfinity);
      for(l <- inside.bot.enteredLabelIndexes(begin,end)) {
        val pL = indexedProjections.project(l)
        val myScore = inside.bot.labelScore(begin, end, l) + outside.bot.labelScore(begin, end, l) - sentProb
        val currentScore = scoresForLocation(pL);
        scoresForLocation(pL) = Numerics.logSum(currentScore,myScore);
      }

      for( (c,v) <- scoresForLocation.activeElements) {
        if(v > pruningThreshold || markedSpans(index)(c)) {
          if(scores(index) eq null) {
            scores(index) = indexedProjections.coarseEncoder.mkSparseVector(Double.NegativeInfinity);
          }
          scores(index)(c) = v;
        }
      }

      for(c <- markedSpans(index)) {
        if(scores(index) == null || scores(index)(c) == Double.NegativeInfinity) {
          println("grrr....");
          println(parser.grammar.index.get(c) + " " + begin + " " + end + tree + " " + inside.bot.labelScore(begin,end,c) + " " + outside.bot.labelScore(begin,end,c) + " " + sentProb)
          //if(begin + 1 != end) error("crashy");
        }
      }
    }
    //println("Density: " + density * 1.0 / scores.length);
    //println("Label Density:" + labelDensity * 1.0 / scores.length / parser.grammar.index.size)
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

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = 0.0;

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
    val projections = ProjectionIndexer(parser.builder.grammar.index,parser.builder.grammar.index,identity[String])
    val factory = new LabeledSpanScorerFactory[String,String,String](parser.builder.withCharts(ParseChart.logProb),projections);
    writeObject(parser.builder.grammar.index,new File(outDir,SPAN_INDEX_NAME));
    writeIterable(mapTrees(factory,transformTrees(treebank.trainTrees),true),new File(outDir,TRAIN_SPANS_NAME))
    writeIterable(mapTrees(factory,transformTrees(treebank.testTrees),false),new File(outDir,TEST_SPANS_NAME))
    writeIterable(mapTrees(factory,transformTrees(treebank.devTrees),false),new File(outDir,DEV_SPANS_NAME))
  }

  def loadParser(loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[ChartParser[String,String,String]]
    oin.close();
    parser;
  }

  def transformTrees(trees: Iterator[(Tree[String],Seq[String])]): IndexedSeq[(BinarizedTree[String], Seq[String])] = {
    val xform = Trees.Transforms.StandardStringTransform;
    val binarize = Trees.xBarBinarize _;
    val binarizedAndTransformed = (for {
      (tree, words) <- trees
    } yield (binarize(xform(tree)),words)).toIndexedSeq

    val chainRemover = new UnaryChainRemover[String];

    val (dechained,chainReplacer) = chainRemover.removeUnaryChains(binarizedAndTransformed.iterator);

    dechained.toIndexedSeq
  }

  def mapTrees(factory: LabeledSpanScorerFactory[String,String,String], trees: IndexedSeq[(BinarizedTree[String],Seq[String])], useTree: Boolean) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { case (tree,words) =>
      println(words);
      try {
        val scorer = if(useTree) factory.mkSpanScorerWithTree(tree,words) else factory.mkSpanScorer(words);
        scorer;
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

