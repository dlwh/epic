package scalanlp.parser
package discrim


import scalala.tensor.dense.DenseVector
import scalanlp.parser.projections._
import scalanlp.optimize._

import scalanlp.trees._
import InsideOutside._

import scalanlp.util._;
import Log._;

import scalala.library.Library._
import scalala.tensor.sparse.SparseVector
;


abstract class AbstractDiscriminativeObjective[L,L2,W](
  trees: IndexedSeq[TreeInstance[L,W]],
  val indexedProjections: GrammarProjections[L,L2],
  openTags: Set[L2],
  closedWords: Set[W]) extends BatchDiffFunction[DenseVector[Double]] with Logged {

  def extractParser(weights: DenseVector[Double]):Parser[L,W];
  def initialWeightVector:DenseVector[Double]

  protected type Builder
  protected type Counts
  protected def builder(weights: DenseVector[Double]):Builder
  protected def emptyCounts(b: Builder): Counts
  protected def expectedCounts(b: Builder, t: BinarizedTree[L], w: Seq[W], scorer: SpanScorer[L]):Counts
  protected def sumCounts(c1: Counts, c2: Counts):Counts
  /** Should return -logProb and the derivative */
  protected def countsToObjective(c: Counts):(Double,DenseVector[Double])

  private var numFailures = 0;

  def calculate(weights: DenseVector[Double], sample: IndexedSeq[Int]) = {
    val parser = builder(weights);
    val trees = sample.map(this.trees);
    val startTime = System.currentTimeMillis();
    val ecounts = trees.par.view.map{ treeWordsScorer =>
      val localIn = System.currentTimeMillis();
      val TreeInstance(id,tree,words,spanScorer) = treeWordsScorer;
      val res = try {
        expectedCounts(parser,tree,words,spanScorer)
      } catch {
        case e => println("Error in parsing: " + words + e);
        e.printStackTrace()
        throw e;
      }
      println(id,words,sample.length)
      res
    } reduce {
      sumCounts(_:Counts,_:Counts)
    };
    val finishTime = System.currentTimeMillis() - startTime;

    log(INFO)("Parsing took: " + finishTime / 1000.0)
    val (obj,grad) = countsToObjective(ecounts);
    (obj,grad)

  }

  val fullRange = (0 until trees.length);


  protected def weightsToLexicon(indexedFeatures: FeatureIndexer[L2,W], weights: DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(openTags, closedWords, weights, indexedFeatures);
    lexicon;
  }

  protected def weightsToGrammar(indexedFeatures: FeatureIndexer[L2,W], weights: DenseVector[Double]):Grammar[L2] = {
    val grammar =  FeaturizedGrammar(weights,indexedFeatures)
    grammar;
  }


  def expectedCountsToFeatureVector[L,W](indexedFeatures: FeatureIndexer[L,W], ecounts: ExpectedCounts[W]) = {
    val result = indexedFeatures.mkDenseVector();

    def sumVectorIntoResults(vec: SparseVector[Double], v: Double) {
      var i = 0
      while (i < vec.nonzeroSize) {
        result(vec.data.indexAt(i)) += vec.data.valueAt(i) * v
        i += 1
      }
    }

    // rules
    for((r,v) <- ecounts.ruleCounts.pairsIteratorNonZero)
      sumVectorIntoResults(indexedFeatures.featuresFor(r),v)

    // lex

    for( (a,ctr) <- ecounts.wordCounts; (w,v) <- ctr.nonzero.pairs) {
      val vec = indexedFeatures.featuresFor(a,w)
      sumVectorIntoResults(vec, v)
    }

    result;
  }


}


