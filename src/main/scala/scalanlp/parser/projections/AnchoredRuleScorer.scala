package scalanlp.parser
package projections

import scalanlp.collection.mutable.TriangularArray
import scalanlp.trees.{Trees, BinarizedTree, Tree, DenseTreebank}
import scalanlp.io.FileIterable
import java.io._
import actors.Actor
import scalanlp.util.{HeapDump, Index}
import scalanlp.tensor.sparse.OldSparseVector
import collection.parallel.immutable.ParSeq
;


/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRuleScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                     val indexedProjections: GrammarProjections[C,L],
                                     pruningThreshold: Double = -7)
        extends ChartDrivenScorerFactory[C,L,W](parser,indexedProjections,pruningThreshold) {

  type MyScorer = AnchoredRuleScorer[C];
  private def normalize(ruleScores: OldSparseVector, totals: OldSparseVector):OldSparseVector = {
    if(ruleScores eq null) null
    else {
      val r = new OldSparseVector(ruleScores.length,ruleScores.default,ruleScores.activeSize)
      for( (rule,score) <- ruleScores.activeIterator) {
        val parent = indexedProjections.labels.coarseIndex(indexedProjections.rules.coarseIndex.get(rule).parent)
        r(rule) = (score - totals(parent))
      }
      r
    }
  }

  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores,logTotalsUnaries,binaryScores,logTotals) = ruleData;
    val normUnaries:Array[OldSparseVector] = for((ruleScores,totals) <- unaryScores zip logTotalsUnaries) yield {
      normalize(ruleScores, totals)
    }

    val normBinaries:Array[Array[OldSparseVector]] = for ((splits,totals) <- binaryScores zip logTotals) yield {
      if(splits eq null) null
      else for(ruleScores <- splits) yield normalize(ruleScores,totals)
    }
    new AnchoredRuleScorer(lexicalScores, normUnaries, normBinaries);
  }

}



/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRulePosteriorScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                     indexedProjections: GrammarProjections[C,L],
                                     pruningThreshold: Double = -7)
        extends ChartDrivenScorerFactory[C,L,W](parser,indexedProjections,pruningThreshold) {

  type MyScorer = AnchoredRuleScorer[C];
  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores, _, binaryScores, _) = ruleData;
    new AnchoredRuleScorer(lexicalScores, unaryScores, binaryScores);
  }

}


@SerialVersionUID(3)
class AnchoredRuleScorer[L](spanScores: Array[OldSparseVector], // triangular index -> label -> score
                         // (begin,end) -> rule -> score
                         unaryScores: Array[OldSparseVector],
                         // (begin,end) -> (split-begin) -> rule -> score
                         binaryScores: Array[Array[OldSparseVector]]) extends SpanScorer[L] with Serializable {

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else forSpan(rule)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val forSpan = binaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else {
      val forSplit = forSpan(split - begin)
      if(forSplit eq null) Double.NegativeInfinity
      else forSplit(rule)
    }
  }
  def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
    val scores = spanScores(TriangularArray.index(begin, end))
    if(scores ne null) scores(tag)
    else Double.NegativeInfinity
  }
}

object ProjectTreebankToVarGrammar {
  val TRAIN_SPANS_NAME = "train.spans.ser"
  val DEV_SPANS_NAME = "dev.spans.ser"
  val TEST_SPANS_NAME = "test.spans.ser"
  val SPAN_INDEX_NAME = "spanindex.ser"
  def main(args: Array[String]) {
    Actor.actor {
      def memoryString = {
        val r = Runtime.getRuntime;
        val free = r.freeMemory / (1024 * 1024);
        val total = r.totalMemory / (1024 * 1024);
        ((total - free) + "M used; " + free  + "M free; " + total  + "M total");
      }
      Actor.loop {
        Thread.sleep(60 * 1000);
//        HeapDump.dumpHeap("heap.dump");
        println(memoryString);
      }

    }
    val parser = loadParser(new File(args(0)));
    val coarseParser = ProjectTreebankToLabeledSpans.loadParser(new File(args(1)));
    val treebank = ProcessedTreebank(TreebankParams(new File(args(2)),maxLength=10000),SpanParams(new File(args(3))));
    val outDir = new File(args(4));
    outDir.mkdirs();
    val projections = GrammarProjections(coarseParser.builder.grammar,parser.builder.grammar, {(x: (String,Int))=>x._1})
    val factory = new AnchoredRuleScorerFactory[String,(String,Int),String](parser.builder.withCharts(ParseChart.logProb),projections, -5);
    writeObject(parser.builder.grammar.labelIndex,new File(outDir,SPAN_INDEX_NAME));
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


  def mapTrees(factory: AnchoredRuleScorerFactory[String,(String,Int),String], trees: IndexedSeq[TreeInstance[String,String]], useTree: Boolean): Iterable[SpanScorer[String]] = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { (ti:TreeInstance[String,String]) =>
      val TreeInstance(_,tree,words,scorer) = ti
      println(words);
      try {
        val proj: ProjectingSpanScorer[String, (String, Int)] = new ProjectingSpanScorer(factory.indexedProjections,scorer,true);
        val newScorer = factory.mkSpanScorer(words,proj)
        newScorer
      } catch {
        case e: Exception => e.printStackTrace(); SpanScorer.identity[String];
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