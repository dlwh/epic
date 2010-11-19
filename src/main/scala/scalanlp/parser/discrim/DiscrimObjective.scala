package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.trees._
import scalanlp.config.Configuration
import scalanlp.util.{ConsoleLogging, Log}
import scalanlp.optimize._

import InsideOutside._
import scalanlp.parser.UnaryRuleClosure.UnaryClosureException
import scalala.tensor.counters.LogCounters
import projections.{ProjectionIndexer, CoarseToFineChartBuilder}

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
                            initLexicon: PairedDoubleCounter[L,W],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W])
        extends DiffFunction[Int,DenseVector] {

  def extractViterbiParser(weights: DenseVector) = {
    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val builder = CKYChartBuilder(root, lexicon, grammar);
    val parser = ChartParser(builder);
    parser
  }

  def extractLogProbParser(weights: DenseVector)= {
    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val parser = new CKYChartBuilder[LogProbabilityParseChart, L, W](root, lexicon, grammar, ParseChart.logProb);
    parser
  }

  var numFailures = 0;



  def calculate(weights: DenseVector) = {
    try {
      val parser = new ThreadLocal(extractLogProbParser(weights));
      val ecounts = treesWithCharts.par.fold(new ExpectedCounts[W](parser().grammar)) { (counts, treewords) =>
        val tree = treewords._1;
        val words = treewords._2;
        val scorer = treewords._3;

        val treeCounts = treeToExpectedCounts(parser().grammar,parser().lexicon,tree,words);
        val wordCounts = wordsToExpectedCounts(words, parser(), scorer);
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
        if(numFailures > 20) throw ex;
        ex.printStackTrace();
        (Double.PositiveInfinity,indexedFeatures.mkDenseVector(0.0));
    }


  }

  val indexedFeatures: FeatureIndexer[L,W] = FeatureIndexer(feat,trees);

  private val indexedProjections = new ProjectionIndexer(coarseParser.grammar.index,
    indexedFeatures.labelIndex, identity[L] _)

  val treesWithCharts = trees.par.map { case (tree,words) =>
    val filter = CoarseToFineChartBuilder.coarseSpanScorerFromParser(words,coarseParser, indexedProjections);
    (tree,words,filter)
  }

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

  def wordsToExpectedCounts(words: Seq[W], parser: ChartBuilder[LogProbabilityParseChart,L,W], scorer: SpanScorer) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words, scorer);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  private def treeToExpectedCounts(g: Grammar[L],
                                   lexicon: Lexicon[L,W],
                                   lt: BinarizedTree[L],
                                   words: Seq[W]):ExpectedCounts[W] = {
    val expectedCounts = new ExpectedCounts[W](g)
    val t = lt.map(indexedFeatures.labelIndex);
    var score = 0.0;
    for(t2 <- t.allChildren) {
      t2 match {
        case BinaryTree(a,Tree(b,_),Tree(c,_)) =>
          expectedCounts.binaryRuleCounts.getOrElseUpdate(a).getOrElseUpdate(b)(c) += 1
          score += g.binaryRuleScore(a,b,c);
        case UnaryTree(a,Tree(b,_)) =>
          expectedCounts.unaryRuleCounts.getOrElseUpdate(a)(b) += 1
          score += g.unaryRuleScore(a,b);
        case n@NullaryTree(a) =>
          val w = words(n.span.start);
          expectedCounts.wordCounts.getOrElseUpdate(a)(w) += 1
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


object DiscriminativeTrainer extends ParserTrainer {

  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {

    val (initLexicon,initProductions) = GenerativeParser.extractCounts(trainTrees.iterator);

    val factory = config.readIn[FeaturizerFactory[String,String]]("featurizerFactory",new PlainFeaturizerFactory[String]);
    val featurizer = factory.getFeaturizer(config, initLexicon, initProductions);

    val xbarParser = {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initProductions));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val obj = new DiscrimObjective(featurizer, "", trainTrees.toIndexedSeq,initLexicon, xbarParser);
    val iterationsPerEval = config.readIn("iterations.eval",25);
    val maxIterations = config.readIn("iterations.max",100);
    val maxMStepIterations = config.readIn("iterations.mstep.max",80);
    val regularization = config.readIn("objective.regularization",0.001);
    val opt = new LBFGS[Int,DenseVector](iterationsPerEval,5) with ConsoleLogging;

    val init = obj.initialWeightVector;

    val log = Log.globalLog;
    val reg = DiffFunction.withL2Regularization(obj, regularization);
    val cachedObj = new CachedDiffFunction(reg);
    for( (state,iter) <- opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex;
         if iter != 0 && iter % iterationsPerEval == 0) yield {
       val parser = obj.extractViterbiParser(state.x);
       (iter + "", parser);
    }

  }
}

object DiscrimApproxTest extends ParserTrainer {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {

    val (initLexicon,initProductions) = GenerativeParser.extractCounts(trainTrees.iterator);

    val factory = config.readIn[FeaturizerFactory[String,String]]("featurizerFactory",new PlainFeaturizerFactory[String]);
    val featurizer = factory.getFeaturizer(config, initLexicon, initProductions);


    val xbarParser = {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initProductions));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val obj = new DiscrimObjective(featurizer, "", trainTrees.toIndexedSeq,initLexicon, xbarParser);
    val iterationsPerEval = config.readIn("iterations.eval",25);
    val maxIterations = config.readIn("iterations.max",100);
    val maxMStepIterations = config.readIn("iterations.mstep.max",80);
    val regularization = config.readIn("objective.regularization",0.001);
    val opt = new LBFGS[Int,DenseVector](iterationsPerEval,5) with ConsoleLogging;

    val init = obj.initialWeightVector;

    val log = Log.globalLog;
    val reg = DiffFunction.withL2Regularization(obj, regularization);
    val checking = new GradientCheckingDiffFunction[Int,DenseVector](reg);
    val cachedObj = new CachedDiffFunction(checking);
    for( (state,iter) <- opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex;
         if iter != 0 && iter % iterationsPerEval == 0) yield {
       val parser = obj.extractViterbiParser(state.x);
       (iter + "", parser);
    }

  }
}