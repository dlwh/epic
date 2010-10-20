package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.trees._
import scalanlp.config.Configuration
import scalanlp.util.{ConsoleLogging, Log}
import scalanlp.optimize.{CachedDiffFunction, LBFGS, DiffFunction}
import splitting.StateSplitting
import scalala.tensor.counters.LogCounters
import scalanlp.parser.UnaryRuleClosure.UnaryClosureException;
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
class LatentDiscrimObjective[L,L2,W](feat: Featurizer[L2,W],
                            unsplitRoot: L,
                            trees: IndexedSeq[(BinarizedTree[L],Seq[W])],
                            coarseParser: ChartParser[LogProbabilityParseChart, L, W],
                            openTags: Set[L],
                            splitLabel: L=>Seq[L2],
                            unsplit: (L2=>L)) extends DiffFunction[Int,DenseVector] {

  val root = {
    val splitRoot = splitLabel(unsplitRoot);
    require(splitRoot.length == 1, "Split of root must be length 1");
    splitRoot.head;
  }


  def extractViterbiParser(weights: DenseVector) = {
    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val cc = coarseParser.withCharts[ParseChart.ViterbiParseChart](ParseChart.viterbi);
    val parser = new CoarseToFineParser[ParseChart.ViterbiParseChart,L,L2,W](cc,
      unsplit, root, lexicon, grammar, ParseChart.viterbi);
    //val parser = CKYParser(root, lexicon, grammar);
    parser
  }

  def extractLogProbParser(weights: DenseVector)= {
    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val cc = coarseParser.withCharts[LogProbabilityParseChart](ParseChart.logProb);
    val parser = new CoarseToFineParser[LogProbabilityParseChart,L,L2,W](cc,
      unsplit, root, lexicon, grammar, ParseChart.logProb);
   // val parser = new CKYParser[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb);
    parser
  }

  var numFailures = 0;

  def calculate(weights: DenseVector) = {

    try {
      val parser = new ThreadLocal(extractLogProbParser(weights));
      val ecounts = trees.par.fold(new ExpectedCounts[W](parser().grammar)) { (counts, treewords) =>
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
    }  catch {
      case ex: UnaryClosureException =>
        numFailures += 1;
        if(numFailures > 10) throw ex;
        ex.printStackTrace();
        (Double.PositiveInfinity,indexedFeatures.mkDenseVector(0.0));
    }

  }

  val indexedFeatures: FeatureIndexer[L2,W] = {
    val initGrammar = coarseParser.grammar;
    val initLex = coarseParser.lexicon;
    FeatureIndexer[L,L2,W](feat, initGrammar, initLex, splitLabel);
  }

  val splitOpenTags = {
    for(t <- openTags; s <- splitLabel(t))  yield s;
  }

  def weightsToLexicon(weights: DenseVector) = {
    val grammar = new FeaturizedLexicon(splitOpenTags, weights, indexedFeatures);
    grammar;
  }

  def weightsToGrammar(weights: DenseVector):Grammar[L2] = {
    val grammar =  new FeaturizedGrammar(weights,indexedFeatures)
    grammar;
  }

  def wordsToExpectedCounts(words: Seq[W], parser: ChartParser[LogProbabilityParseChart,L2,W]) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  private def treeToExpectedCounts(g: Grammar[L2],
                                   lexicon: Lexicon[L2,W],
                                   t: BinarizedTree[L],
                                   words: Seq[W]):ExpectedCounts[W] = {
    StateSplitting.expectedCounts(g,lexicon,t.map(splitLabel),words);
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


object LatentDiscriminativeTest extends ParserTester {

  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {

    val (initLexicon,initProductions) = GenerativeParser.extractCounts(trainTrees.iterator);
    val numStates = config.readIn[Int]("discrim.numStates",2);

    val xbarParser = {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initProductions));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYParser[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val factory = config.readIn[FeaturizerFactory[String,String]]("discrim.featurizerFactory",new PlainFeaturizerFactory[String]);
    val featurizer = factory.getFeaturizer(config, initLexicon, initProductions);
    val latentFactory = config.readIn[LatentFeaturizerFactory]("discrim.latentFactory",new SlavLatentFeaturizerFactory());
    val latentFeaturizer = latentFactory.getFeaturizer(featurizer, numStates);


    val openTags = Set.empty ++ {
      for(t <- initLexicon.activeKeys.map(_._1) if initLexicon(t).size > 50) yield t;
    }

    def split(x: String) = {
      if(x.isEmpty) Seq((x,0))
      else for(i <- 0 until numStates) yield (x,i);
    }
    def project(x: (String,Int)) = x._1;

    val obj = new LatentDiscrimObjective(latentFeaturizer, "",
                                         trainTrees.toIndexedSeq,
                                         xbarParser,
                                         openTags,
                                         split _,
                                         project _);


    val iterationsPerEval = config.readIn("iterations.eval",25);
    val maxIterations = config.readIn("iterations.max",300);
    val regularization = config.readIn("objective.regularization",0.01);
    val opt = new LBFGS[Int,DenseVector](iterationsPerEval,5) with ConsoleLogging;

    val init = obj.initialWeightVector;

    val log = Log.globalLog;
    val reg = DiffFunction.withL2Regularization(obj, regularization);
    val cachedObj = new CachedDiffFunction(reg);
    for( (state,iter) <- opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex;
         if iter != 0 && iter % iterationsPerEval == 0) yield {
       val parser = obj.extractViterbiParser(state.x).map { (t:Tree[(String,Int)]) =>
         t.map(_._1);
       };
       (iter + "", parser);
    }

  }
}
