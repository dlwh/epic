package scalanlp.parser
package discrim


import scalala.tensor.dense.DenseVector
import scalanlp.parser.projections._
import scalanlp.optimize._

import scalanlp.trees._
import scalanlp.parser.UnaryRuleClosure.UnaryClosureException
import InsideOutside._


import scalanlp.concurrent.ParallelOps._;

import scalala.Scalala._;
import scalanlp.util._;
import Log._;


abstract class AbstractDiscriminativeObjective[L,L2,W](
  trees: IndexedSeq[TreeInstance[L,W]],
  val indexedProjections: ProjectionIndexer[L,L2],
  openTags: Set[L2],
  closedWords: Set[W]) extends BatchDiffFunction[Int,DenseVector] with Logged {

  def extractParser(weights: DenseVector):Parser[L,W];
  def initialWeightVector:DenseVector

  protected type Builder
  protected type Counts
  protected def builder(weights: DenseVector):Builder
  protected def emptyCounts(b: Builder): Counts
  protected def expectedCounts(b: Builder, t: BinarizedTree[L], w: Seq[W], scorer: SpanScorer[L]):Counts
  protected def sumCounts(c1: Counts, c2: Counts):Counts
  /** Should return -logProb and the derivative */
  protected def countsToObjective(c: Counts):(Double,DenseVector)

  private var numFailures = 0;

  def calculate(weights: DenseVector, sample: IndexedSeq[Int]) = {
    println(norm(weights,2));

    try {
      val parser = builder(weights);
      val trees = sample.map(this.trees);
      val startTime = System.currentTimeMillis();
      val ecounts = trees.par.mapReduce({ treeWordsScorer =>
        val localIn = System.currentTimeMillis();
        val TreeInstance(_,tree,words,spanScorer) = treeWordsScorer;
        try {
          expectedCounts(parser,tree,words,spanScorer)
        } catch {
          case e => println("Error in parsing: " + words + e); throw new RuntimeException("Error parsing " + words,e);
        }
      },{ sumCounts(_:Counts,_:Counts)})
      val finishTime = System.currentTimeMillis() - startTime;

      log(INFO)("Parsing took: " + finishTime / 1000.0)
      val (obj,grad) = countsToObjective(ecounts);
      (obj,grad)
    }  catch {
      case ex: UnaryClosureException =>
        numFailures += 1;
        if(numFailures > 10) throw ex;
        ex.printStackTrace();
        (Double.PositiveInfinity,weights.like);
    }

  }

  val fullRange = (0 until trees.length);


  protected def weightsToLexicon(indexedFeatures: FeatureIndexer[L2,W], weights: DenseVector) = {
    val lexicon = new FeaturizedLexicon(openTags, closedWords, weights, indexedFeatures);
    lexicon;
  }

  protected def weightsToGrammar(indexedFeatures: FeatureIndexer[L2,W], weights: DenseVector):Grammar[L2] = {
    val grammar =  new FeaturizedGrammar(weights,indexedFeatures)
    grammar;
  }


  def expectedCountsToFeatureVector(indexedFeatures: FeatureIndexer[L2,W], ecounts: ExpectedCounts[W]):DenseVector = {
   val result = indexedFeatures.mkDenseVector(0.0);

    // binaries
    for( (a,bvec) <- ecounts.binaryRuleCounts;
         (b,cvec) <- bvec;
         (c,v) <- cvec.activeElements) {
      result += (indexedFeatures.featuresFor(a,b,c) * v);
    }

    // unaries
    for( (a,bvec) <- ecounts.unaryRuleCounts;
         (b,v) <- bvec.activeElements) {
      result += (indexedFeatures.featuresFor(a,b) * v);
    }

    // lex
    for( (a,ctr) <- ecounts.wordCounts;
         (w,v) <- ctr) {
      result += (indexedFeatures.featuresFor(a,w) * v);
    }

    result;
  }


}


