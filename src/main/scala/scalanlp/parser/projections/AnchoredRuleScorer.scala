package scalanlp.parser
package projections

import scalanlp.trees._
import scalanlp.math.Numerics
import scalanlp.collection.mutable.{SparseArray, TriangularArray}
import scalala.tensor.sparse.SparseVector;

import java.io._
import scalanlp.concurrent.ParallelOps._
import scalanlp.trees.DenseTreebank

/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRuleScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                     indexedProjections: ProjectionIndexer[C,L],
                                     pruningThreshold: Double = -7) extends SpanScorer.Factory[W] {

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

  def goldLabels(length: Int, tree: BinarizedTree[C]) = {
    val result = TriangularArray.raw(length,collection.mutable.BitSet());
    if(tree != null) {
      for( t <- tree.allChildren) {
        result(TriangularArray.index(t.span.start,t.span.end)).+=(indexedProjections.coarseIndex(t.label));
      }
    }
    result;
  }

  def buildSpanScorer(inside: ParseChart[L],
                      outside: ParseChart[L],
                      sentProb: Double,
                      scorer: SpanScorer=SpanScorer.identity,
                      tree: BinarizedTree[C] = null):AnchoredRuleScorer = {
    val numProjectedLabels = indexedProjections.coarseIndex.size;
    def projFill[T:ClassManifest](t: =>T) = new SparseArray(numProjectedLabels,t,0);
    def projVector() = {
      val vec = new SparseVector(numProjectedLabels,0);
      vec.default = Double.NegativeInfinity;
      vec
    }

    val gold = goldLabels(inside.length,tree);

    val lexicalScores = Array.fill(inside.length)(projVector())
    val unaryScores = TriangularArray.raw(inside.length+1,null:SparseArray[SparseVector]);
    val logTotals = TriangularArray.raw(inside.length+1,null:SparseVector);
    // so hard!
    val binaryScores = TriangularArray.raw[Array[SparseArray[SparseArray[SparseVector]]]](inside.length+1,null);
    for(begin <- 0 until inside.length; end <- (begin + 1) to inside.length) {
      val numSplits = end - begin;
      if(!inside.enteredLabelIndexes(begin,end).isEmpty) // is there anything to put here?
        binaryScores(TriangularArray.index(begin,end)) = Array.fill(numSplits)(projFill[SparseArray[SparseVector]](null));
    }

    val grammar = parser.grammar;
    for(begin <- 0 until inside.length; end = begin + 1;
        l <- inside.enteredLabelIndexes(begin,end) if parser.grammar.isPreterminal(l)) {
      val accScore = lexicalScores(begin)(indexedProjections.project(l));
      val currentScore = inside.labelScore(begin,end,l) + outside.labelScore(begin,end,l) - sentProb;
      val pL = indexedProjections.project(l)
      if(currentScore > pruningThreshold || gold(TriangularArray.index(begin,end))(pL))
          lexicalScores(begin)(pL) = 0
    }

    for(diff <- 1 to inside.length) {
      for(begin <- 0 until (inside.length - diff + 1)) {
        val end = begin + diff;
        val index = TriangularArray.index(begin,end);

        // do binaries
        for( parent <- inside.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.labelScore(begin,end,parent);
          val pP = indexedProjections.project(parent);

          var logTotal = Double.NegativeInfinity;

          for(split <- (begin+1) until end) {
            lazy val parentArray = if(binaryScores(index)(split-begin)(pP) eq null) {
              binaryScores(index)(split-begin)(pP) = projFill(projVector);
              binaryScores(index)(split-begin)(pP)
            } else {
              binaryScores(index)(split-begin)(pP);
            }

            // P(sAt->sBu uCt | sAt) \propto \sum_{latent} O(A-x,s,t) r(A-x ->B-y C-z) I(B-y,s,u) I(C-z, s, u)
            for(b <- inside.enteredLabelIndexes(begin,split)) {
              val cRules = grammar.binaryRulesByIndexedParent(parent)(b);
              val pB = indexedProjections.project(b);
              var i = 0
              val bScore = inside.labelScore(begin, split, b)
              while(i < cRules.used) {
                val c = cRules.index(i);
                val ruleScore = cRules.data(i)
                val pC = indexedProjections.project(c);
                val currentScore = (bScore + inside.labelScore(split,end,c)
                        + parentScore + ruleScore + scorer.scoreBinaryRule(begin,split,end,parent,b,c) - sentProb);
                if(currentScore > pruningThreshold || (gold(index)(pP) && gold(TriangularArray.index(begin,split))(pB) && gold(TriangularArray.index(split,end))(pC))) {
                  val accScore = parentArray.getOrElseUpdate(pB,projVector())(pC);
                  if(gold(index)(pP)
                          && gold(TriangularArray.index(begin,split))(pB)
                          && gold(TriangularArray.index(split,end))(pC)
                          && currentScore == Double.NegativeInfinity && accScore == Double.NegativeInfinity) {
                    parentArray(pB)(pC) = Numerics.logSum(accScore,-100);
                    logTotal = Numerics.logSum(logTotal,-100);
                  } else {
                    parentArray(pB)(pC) = Numerics.logSum(accScore,currentScore);
                    logTotal = Numerics.logSum(logTotal,currentScore);
                  }
                }
                i+=1;
              }
            }


          }
          // do unaries. Similar to above
          lazy val parentArray = if(unaryScores(index) eq null) {
            unaryScores(index) = projFill(projVector());
            unaryScores(index).getOrElseUpdate(pP,projVector());
          } else {
            unaryScores(index).getOrElseUpdate(pP,projVector())
          }
          for( (c,ruleScore) <- parser.grammar.unaryRulesByIndexedParent(parent)) {
            val score = ruleScore + inside.labelScore(begin,end,c) + parentScore +
                    scorer.scoreUnaryRule(begin,end,parent,c) - sentProb;
            val pC = indexedProjections.project(c);
            if(pP != pC && (score > pruningThreshold || (gold(index)(pP) && gold(index)(pC)))) {
              assert(parentArray != null, unaryScores(index)(pP));
              val accScore = parentArray(pC);
              if(gold(index)(pP) && gold(index)(pC) && score == Double.NegativeInfinity && accScore == Double.NegativeInfinity) {
                parentArray(pC) = -100;
                logTotal = Numerics.logSum(logTotal,-100);
              } else {
                parentArray(pC) = Numerics.logSum(accScore,score);
                logTotal = Numerics.logSum(logTotal,score);
              }
            }
          }

          if(logTotal != Double.NegativeInfinity) {
            if(logTotals(index) eq null) {
              logTotals(index) = projVector;
            }
            logTotals(index)(pP) = logTotal;
          }

        }


      }

    }

    new AnchoredRuleScorer(lexicalScores, unaryScores, binaryScores, logTotals);
  }

}

@serializable
@SerialVersionUID(1)
class AnchoredRuleScorer(lexicalScores: Array[SparseVector], // begin -> label -> score
                         // (begin,end) -> parent -> child -> score
                         unaryScores: Array[SparseArray[SparseVector]],
                         // (begin,end) -> (split-begin) -> parent -> lchild -> rchild -> score
                         // so many arrays.
                         binaryScores: Array[Array[SparseArray[SparseArray[SparseVector]]]],
                         logTotals: Array[SparseVector] // sum of scores for bianry and unary scores.
                        ) extends SpanScorer {
  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else if(forSpan(parent) eq null) Double.NegativeInfinity
    else if(logTotals(TriangularArray.index(begin,end)) eq null)  Double.NegativeInfinity
    else {
      val r= forSpan(parent)(child) - logTotals(TriangularArray.index(begin,end))(parent);
      if(logTotals(TriangularArray.index(begin,end))(parent) == Double.NegativeInfinity) Double.NegativeInfinity
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
        if(forParent eq null) Double.NegativeInfinity
        else {
          val r = forParent(leftChild)(rightChild) - logTotals(TriangularArray.index(begin,end))(parent);
          if(logTotals(TriangularArray.index(begin,end))(parent) == Double.NegativeInfinity) Double.NegativeInfinity
          else r

        }
      }
    }
  }
  def scoreLexical(begin: Int, end: Int, tag: Int): Double = {
    lexicalScores(begin)(tag);
  }
}

object ProjectTreebankToAnchoredRules {
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
    val factory = new AnchoredRuleScorerFactory[String,String,String](parser.builder.withCharts(ParseChart.logProb), projections);
    writeObject(parser.builder.grammar.index,new File(outDir,SPAN_INDEX_NAME));
    ProjectTreebankToLabeledSpans.writeIterable(mapTrees(factory,treebank.trainTrees.toIndexedSeq),new File(outDir,TRAIN_SPANS_NAME))
    ProjectTreebankToLabeledSpans.writeIterable(mapTrees(factory,treebank.testTrees.toIndexedSeq),new File(outDir,TEST_SPANS_NAME))
    ProjectTreebankToLabeledSpans.writeIterable(mapTrees(factory,treebank.devTrees.toIndexedSeq),new File(outDir,DEV_SPANS_NAME))
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
        case e: Exception => e.printStackTrace(); throw e; //SpanScorer.identity;
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
