package scalanlp.parser
package discrim

import scalanlp.trees.UnaryChainRemover.ChainReplacer

import scalanlp.optimize.FirstOrderMinimizer.OptParams;


import scalala.tensor.Vector;
import scalanlp.collection.mutable.TriangularArray
import scalanlp.util._
import scalanlp.trees._
import scalanlp.parser.InsideOutside.ExpectedCounts
import scalanlp.parser.ParseChart.LogProbabilityParseChart
import structpred._;
import edu.berkeley.nlp.util.CounterInterface;
import scala.collection.JavaConversions._
import java.io.{File, FileWriter}
import scalala.library.Library
import Library.sum
import scalala.tensor.::
import projections.{GrammarProjections, ProjectingSpanScorer, ProjectionIndexer}


/**
 *
 * @author dlwh
 */
object StructuredPipeline extends ParserPipeline {

  protected val paramManifest = manifest[Params];
  case class Params(parser: ParserParams.BaseParser[String],
                    opt: OptParams,
                    featurizerFactory: FeaturizerFactory[String,String] = new PlainFeaturizerFactory[String],
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 30,
                    iterPerValidate: Int = 10,
                    C: Double = 1E-4,
                    epsilon: Double = 1E-2,
                    mira: Boolean = false,
                    smoMiniBatch: Boolean = true,
                    innerSmoIters:Int = 10,
                    minDecodeToSmoTimeRatio:Double = 0.0,
                    miniBatchSize: Int = 1);

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {
    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);

    val xbarParser = {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }


    val projections = params.parser.optParser.map(p => GrammarProjections.identity(p.grammar)) getOrElse {
      GrammarProjections.identity(xbarParser.grammar);
    }
    val indexedProjections = GrammarProjections.identity(xbarParser.grammar)

    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);

    val openTags = Set.empty ++ {
      for(t <- initLexicon.domain._1 if initLexicon(t, ::).size > 50) yield t;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 10).map(_._1);
    }


    val obj = new DiscrimObjective(featurizer, trainTrees, xbarParser, openTags, closedWords);
    val weights = obj.initialWeightVector;
    val pp = obj.extractMaxParser(weights);
    val model = new ParserLinearModel[ParseChart.LogProbabilityParseChart](projections, obj);

    NSlackSVM.SvmOpts.smoMiniBatch = params.smoMiniBatch;
    NSlackSVM.SvmOpts.innerSmoIters = params.innerSmoIters;
    NSlackSVM.SvmOpts.minDecodeToSmoTimeRatio = params.minDecodeToSmoTimeRatio;
    NSlackSVM.SvmOpts.miniBatchSize = params.miniBatchSize;

    val learner = if (params.mira) new MIRA[TreeInstance[String,String]](true,false,params.C)
                  else new NSlackSVM[TreeInstance[String,String]](params.C,params.epsilon,10, new NSlackSVM.SvmOpts)
    val finalWeights = learner.train(model.denseVectorToCounter(weights),model, trainTrees, params.maxIterations);
    val parser = model.getParser(finalWeights);

    Iterator(("Base",obj.extractParser(weights)),("Structured",parser));
  }

  import java.{util=>ju}


  class ParserLinearModel[Chart[X]<:ParseChart[X]](projections: GrammarProjections[String,String],
                                                   obj: DiscrimObjective[String,String]) extends LossAugmentedLinearModel[TreeInstance[String,String]] {

    def startIteration(p1: Int) {}

    type Datum = TreeInstance[String,String];

    def getParser(weights: CounterInterface[java.lang.Integer]) = {
      val parser = obj.extractMaxParser(weightsToDenseVector(weights));
      val realParser = new SimpleChartParser(parser.builder, new SimpleViterbiDecoder[String,String](parser.builder.grammar), projections, false);
      realParser
    }

    val timeIn : Long = System.currentTimeMillis();

    override def getLossAugmentedUpdateBundle(datum: Datum, lossWeight: Double): UpdateBundle = {
      getLossAugmentedUpdateBundleBatch(ju.Collections.singletonList(datum), 1, lossWeight).get(0);
    }

    override def getLossAugmentedUpdateBundleBatch(data: ju.List[Datum], numThreads: Int,
                                           lossWeight: Double): ju.List[UpdateBundle] = {
      val parser = getParser(weights);
      val logSumBuilder = parser.builder.withCharts(ParseChart.logProb);
      data.toIndexedSeq.par.map { (ti:TreeInstance[String,String]) =>
        val TreeInstance(_,goldTree,words,coarseFilter) = ti;
        val lossScorer = lossAugmentedScorer(lossWeight,projections.labels,goldTree);
        val scorer = SpanScorer.sum(coarseFilter, lossScorer)

//        val logSumCounts = obj.wordsToExpectedCounts(words,logSumBuilder,coarseFilter);

        val guessTree = parser.bestParse(words,scorer);
        val (goldCounts,_) = treeToExpectedCounts(parser,ti);
//        val (guessCounts,loss) = treeToExpectedCounts(parser,guessTree,words,unweightedLossScorer);
        val guessCounts = obj.wordsToExpectedCounts(words,logSumBuilder,coarseFilter);

        val goldFeatures = expectedCountsToFeatureVector(obj.indexedFeatures, goldCounts);
        val guessFeatures = expectedCountsToFeatureVector(obj.indexedFeatures, guessCounts);

        val bundle = new UpdateBundle;
        bundle.gold = denseVectorToCounter(goldFeatures);
        bundle.guess = denseVectorToCounter(guessFeatures);
        bundle.loss = 0.0 //loss;

        bundle
      }.seq
    }


    override def getUpdateBundle(datum: Datum): UpdateBundle = getLossAugmentedUpdateBundle(datum, 0.0);
    override def getUpdateBundleBatch(datum: ju.List[Datum], numThreads: Int): ju.List[UpdateBundle] = {
      getLossAugmentedUpdateBundleBatch(datum,numThreads,0.0);

    }

    @scala.reflect.BeanProperty
    var weights: CounterInterface[java.lang.Integer] = _;

    def weightsToDenseVector(weights: CounterInterface[java.lang.Integer]) = {
      val vector = obj.indexedFeatures.mkDenseVector(0.0);
      for(entry <- weights.entries) {
        vector(entry.getKey.intValue) = entry.getValue.doubleValue;
      }
      vector
    }

    def denseVectorToCounter(vec: Vector[Double]) = {
      val res = new edu.berkeley.nlp.util.Counter[java.lang.Integer]();
      for( (i,v) <- vec.nonzero.pairs.iterator) {
        res.setCount(i,v);
      }
      res
    }

  }

  def lossAugmentedScorer[L](lossWeight: Double, projections: ProjectionIndexer[L,L], goldTree: Tree[L]): SpanScorer[L] = {
    val gold = new TriangularArray(goldTree.span.end+1,collection.mutable.BitSet());
    for( t <- goldTree.allChildren) try {
      gold(t.span.start,t.span.end) += projections.project(projections.fineIndex(t.label));
    } catch {
      case e: Throwable => throw new RuntimeException("rrrr " + t.label + " " + projections.fineIndex(t.label), e);
    }

    val scorer = new SpanScorer[L] {
      def scoreSpan(begin: Int, end: Int, tag: Int) = if(begin + 1 == end || gold(begin,end)(tag)) 0.0 else lossWeight
      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0
      def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0
    }

    scorer
  }

  def treeToExpectedCounts[L,W](parser: SimpleChartParser[L,L,W],
                                treeInstance: TreeInstance[L,W]):(ExpectedCounts[W],Double) = {
    val TreeInstance(_,t,words,spanScorer) = treeInstance;
    val g = parser.builder.grammar;
    val lexicon = parser.builder.lexicon;
    val expectedCounts = new ExpectedCounts[W](g)
    var score = 0.0;
    var loss = 0.0;
    for(t2 <- t.allChildren) {
      t2 match {
        case BinaryTree(a,bt@ Tree(b,_),Tree(c,_)) =>
          val r = g.index(BinaryRule(a,b,c))
          expectedCounts.ruleCounts(r) += 1
          score += g.ruleScore(r);
          loss += spanScorer.scoreBinaryRule(t2.span.start,bt.span.end, t2.span.end, r) + spanScorer.scoreSpan(t2.span.start,t2.span.end,r)
        case UnaryTree(a,Tree(b,_)) =>
          val r = g.index(UnaryRule(a,b))
          expectedCounts.ruleCounts(r) += 1
          score += g.ruleScore(r);
          loss += spanScorer.scoreUnaryRule(t2.span.start,t2.span.end,r);
        case n@NullaryTree(a) =>
          val aI = g.labelIndex(a)
          val w = words(n.span.start);
          expectedCounts.wordCounts.getOrElseUpdate(aI)(w) += 1
          score += lexicon.wordScore(g.labelIndex.get(aI), w);
          loss += spanScorer.scoreSpan(t2.span.start,t2.span.end,aI);

      }
    }
    expectedCounts.logProb = score;
    (expectedCounts,loss);
  }

  def expectedCountsToFeatureVector[L,W](indexedFeatures: FeatureIndexer[L,W], ecounts: ExpectedCounts[W]) = {
    val result = indexedFeatures.mkSparseVector();

    // binaries
    for((r,v) <- ecounts.ruleCounts.pairsIteratorNonZero)
      result += (indexedFeatures.featuresFor(r) * v);

    // lex
    for( (a,ctr) <- ecounts.wordCounts; (w,v) <- ctr.nonzero.pairs) {
      result += (indexedFeatures.featuresFor(a,w) * v);
    }

    result;
  }

}
