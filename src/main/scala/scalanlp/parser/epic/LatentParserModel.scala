package scalanlp.parser.epic

import scalanlp.parser._
import scalanlp.parser.InsideOutside.{ExpectedCounts=>TrueCounts}
import projections.{ProjectingSpanScorer, GrammarProjections}
import splitting.StateSplitting
import ParseChart.LogProbabilityParseChart
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalanlp.optimize.FirstOrderMinimizer.OptParams
import scalanlp.parser.ParseEval.Statistics
import scalala.library.Library
import scalala.tensor.::
import scalala.library.Library._
import scalanlp.optimize.{RandomizedGradientCheckingFunction, BatchDiffFunction, FirstOrderMinimizer, CachedBatchDiffFunction}

class LatentParserModel[L,L2,W](featurizer: Featurizer[L2,W],
                                root:L2,
                                proj: GrammarProjections[L,L2],
                                knownTagWords: Iterable[(L2,W)],
                                openTags: Set[L2],
                                closedWords: Set[W]) extends AbstractParserModel[L,L2,W] {
  type Inference = LatentParserInference[L,L2, W]

  val indexedFeatures: FeatureIndexer[L2, W]  = FeatureIndexer(featurizer, knownTagWords, proj)

  def emptyCounts = ParserExpectedCounts[W](new TrueCounts(proj.rules.fineIndex.size,proj.labels.fineIndex.size))

  def numFeatures = indexedFeatures.index.size

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val grammar = FeaturizedGrammar(weights,indexedFeatures)
    val lexicon = new FeaturizedLexicon(openTags, closedWords, weights, indexedFeatures)
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb)

    new LatentParserInference(parser, proj)
  }

  def extractParser(weights: DenseVector[Double]):ChartParser[L,L2,W] = {
    new SimpleChartParser(inferenceFromWeights(weights).builder,new MaxConstituentDecoder[L,L2,W](proj),proj)
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    val counts = expectedCountsToFeatureVector(ecounts.trueCounts)
    (ecounts.loss,counts)
  }

  def expectedCountsToFeatureVector(ecounts: TrueCounts[W]) = {
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
    for( (ctr,a) <- ecounts.wordCounts.iterator.zipWithIndex; (w,v) <- ctr.nonzero.pairs) {
      val vec = indexedFeatures.featuresFor(a,w)
      sumVectorIntoResults(vec, v)
    }

    result;
  }
}

case class LatentParserInference[L,L2,W](builder: ChartBuilder[LogProbabilityParseChart,L2,W],
                                         projections: GrammarProjections[L,L2]) extends ParserInference[L,L2,W] {

  // E[T-z|T,params]
  def goldCounts(ti: TreeInstance[L,W], spanScorer: SpanScorer[L]) = {
    val projected = new ProjectingSpanScorer(projections,spanScorer)
    val ecounts = new StateSplitting(
      builder.grammar,
      builder.lexicon).expectedCounts(ti.tree.map(projections.labels.refinementsOf _),ti.words,projected)

    new ParserExpectedCounts(ecounts)
  }

}


object LatentPipeline extends ParserPipeline {
  case class Params(parser: ParserParams.BaseParser[String],
                    opt: OptParams,
                    numStates: Int= 2,
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 202,
                    iterPerValidate: Int = 10);
  protected val paramManifest = manifest[Params]


  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i)
  }

  def unsplit(x: (String,Int)) = x._1

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]], validate: (Parser[String, String]) => Statistics, params: Params) = {
    import params._
    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees)

    val xbarParser = parser.optParser.getOrElse {
      println("building a parser from scratch...")
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries))
      val lexicon = new SimpleLexicon(initLexicon)
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb)
    }
    val feat = new SumFeaturizer(new SimpleFeaturizer[String,String], new WordShapeFeaturizer[String](initLexicon))
    val latentFeat = new SubstateFeaturizer(feat)
    val indexedProjections = GrammarProjections(xbarParser.grammar, split(_:String,numStates), unsplit)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t, numStates).iterator ) yield t2
    }

    val knownTagWords = {
      for( (t,w) <- xbarParser.lexicon.knownTagWords.toIterable; t2 <- split(t,numStates)) yield (t2,w)
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1)
    }

    val model = new LatentParserModel[String,(String,Int),String](latentFeat, ("",0), indexedProjections, knownTagWords, openTags, closedWords)

    val obj = new ModelObjective(model,trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val init = obj.initialWeightVector

    type OptState = FirstOrderMinimizer[DenseVector[Double],BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState,Int) ) {
      val (state,iter) = pair
      val weights = state.x
      if(iter % iterPerValidate == 0) {
        println("Validating...")
        val parser = model.extractParser(weights)
        println(validate(parser))
      }
    }

    for( (state,iter) <- params.opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _)
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = model.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e
    }
  }
}