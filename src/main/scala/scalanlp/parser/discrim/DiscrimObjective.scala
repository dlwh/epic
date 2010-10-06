package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalanlp.trees._
import scalanlp.config.Configuration
import scalanlp.optimize.{LBFGS, DiffFunction}
import scalanlp.util.{ConsoleLogging, Log};
import InsideOutside._;
import ParseChart.LogProbabilityParseChart;
import scalanlp.concurrent.ParallelOps._;
import scalanlp.concurrent.ThreadLocal;
import scalala.Scalala._;
import scalala.tensor.counters.Counters._
import scalanlp.util._;

/**
 * 
 * @author dlwh
 */
class DiscrimObjective[L,W](feat: Featurizer[L,W],
                            root: L,
                            trees: IndexedSeq[(BinarizedTree[L],Seq[W])],
                            initLexicon: PairedDoubleCounter[L,W]) extends DiffFunction[Int,DenseVector] {
  def extractParser(weights: DenseVector)= {
    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val parser = new CKYParser[LogProbabilityParseChart, L, W](root, lexicon, grammar, ParseChart.logProb);
    parser
  }

  def calculate(weights: DenseVector) = {

    val parser = new ThreadLocal(extractParser(weights));
    val ecounts = indexedTrees.par.fold(new ExpectedCounts[W](parser().grammar)) { (counts, treewords) =>
      val tree = treewords._1;
      val words = treewords._2;

      val treeCounts = treeToExpectedCounts(parser().grammar,parser().lexicon,tree,words);
      val wordCounts = wordsToExpectedCounts(words, parser());
      counts += treeCounts
      counts -= wordCounts;

      counts

    } { (ecounts1, ecounts2) =>
      ecounts1 += ecounts2

      ecounts1
    }


    val grad = -expectedCountsToFeatureVector(ecounts)
    println((norm(grad,2), ecounts.logProb));

    (-ecounts.logProb, grad value);
  }

  val indexedFeatures: FeatureIndexer[L,W] = FeatureIndexer(feat,trees);
  val indexedTrees = for( (t,w) <- trees) yield (t.map(indexedFeatures.labelIndex),w);

  val openTags = Set.empty ++ {
    for( t <- initLexicon.rows.map(_._1) if initLexicon(t).size > 50)  yield t;
  }

  def weightsToLexicon(weights: DenseVector) = {
    val grammar = new FeaturizedLexicon(openTags, weights, indexedFeatures);
    grammar;
  }

  def weightsToGrammar(weights: DenseVector):Grammar[L] = {
    val grammar =  new FeaturizedGrammar(weights,indexedFeatures)
    grammar;
  }

  def wordsToExpectedCounts(words: Seq[W], parser: ChartParser[LogProbabilityParseChart,L,W]) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  private def treeToExpectedCounts(g: Grammar[L],
                                   lexicon: Lexicon[L,W],
                                   t: BinarizedTree[Int],
                                   words: Seq[W]):ExpectedCounts[W] = {
    val expectedCounts = new ExpectedCounts[W](g)
    var score = 0.0;
    for(t2 <- t.allChildren) {
      t2 match {
        case BinaryTree(a,Tree(b,_),Tree(c,_)) =>
          expectedCounts.binaryRuleCounts(a)(b)(c) += 1
          score += g.binaryRuleScore(a,b,c);
        case UnaryTree(a,Tree(b,_)) =>
          expectedCounts.unaryRuleCounts(a)(b) += 1
          score += g.unaryRuleScore(a,b);
        case n@NullaryTree(a) =>
          val w = words(n.span.start);
          expectedCounts.wordCounts(a)(w) += 1
          score += lexicon.wordScore(g.index.get(a), w);
      }
    }
    expectedCounts.logProb = score;
    expectedCounts;
  }

  def expectedCountsToFeatureVector(ecounts: ExpectedCounts[W]):DenseVector = {
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

  def initialWeightVector = {
    val result = indexedFeatures.mkDenseVector(0.0);
    for(f <- 0 until result.size) {
      result(f) = indexedFeatures.initialValueFor(f);
    }
    result;
  }


}


object DiscriminativeTest extends ParserTester {

  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {

    val (initLexicon,initProductions) = GenerativeParser.extractCounts(trainTrees.iterator);

    val featurizer = new SimpleFeaturizer[String,String]();//new SmartLexFeaturizer(initLexicon);

    val obj = new DiscrimObjective(featurizer, "", trainTrees.toIndexedSeq,initLexicon);
    val iterationsPerEval = config.readIn("iterations.eval",25);
    val maxIterations = config.readIn("iterations.max",100);
    val maxMStepIterations = config.readIn("iterations.mstep.max",80);
    val opt = new LBFGS[Int,DenseVector](iterationsPerEval,5) with ConsoleLogging;

    val init = obj.initialWeightVector;

    val log = Log.globalLog;
    for( (state,iter) <- opt.iterations(obj,init).take(maxIterations).zipWithIndex;
         if iter != 0 && iter % iterationsPerEval == 0) yield {
       val parser = obj.extractParser(state.x);
       (iter + "", parser);
    }

  }
}