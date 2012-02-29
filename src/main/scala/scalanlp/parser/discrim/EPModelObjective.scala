package scalanlp.parser
package discrim

import scalanlp.trees._
import scalanlp.parser.InsideOutside.ExpectedCounts
import scalanlp.parser.ParseChart._
import scalanlp.util.logging.ConfiguredLogging
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.{DenseVectorCol, DenseVector}
import discrim.AbstractDiscriminativeObjective._
import splitting.StateSplitting
import projections.GrammarProjections._
import scalala.library.Library._
import scalala.library.Library
import scalanlp.parser.ParseEval.Statistics
import scalanlp.optimize.FirstOrderMinimizer.OptParams
import java.io.File
import scalanlp.util._
import scalala.tensor.{Counter, Counter2, ::}
import projections.{ProjectingSpanScorer, GrammarProjections}
import scalanlp.optimize.{FirstOrderMinimizer, CachedBatchDiffFunction, BatchDiffFunction}

class EPModelObjective[L,W](models: Seq[DiscEPModel[L,W]],
                            trees: IndexedSeq[TreeInstance[L,W]],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                            maxEPIterations: Int = 1) extends BatchDiffFunction[DenseVector[Double]] with ConfiguredLogging {

  // preliminaries
  private def numModels = models.length
  protected type Builder = EPParser[L,W]
  protected type Counts = (Double,Seq[ExpectedCounts[W]])



  def calculate(weights: DenseVector[Double], sample: IndexedSeq[Int]) = {
    val inTime = System.currentTimeMillis()
    val parser = extractParser(weights);
    val startTime = System.currentTimeMillis();
    val trees = sample.map(this.trees)
    val ecounts = trees.par.map{ instance =>
      val TreeInstance(id,tree,words,spanScorer) = instance;
      val res = try {
        expectedCounts(parser,tree,words,spanScorer)
      } catch {
        case e => println("Error in parsing: " + words + e);
        e.printStackTrace()
        emptyCounts(parser)
        //        throw e;
      }
      res
    } reduce {
      sumCounts(_:Counts,_:Counts)
    };
    val finishTime = System.currentTimeMillis() - startTime;

    log.info("Parsing took: " + finishTime / 1000.0)
    val (obj,grad) = countsToObjective(ecounts);
    val outTime = System.currentTimeMillis()
    log.info("Everything took: " + (outTime - inTime) / 1000.0)
    (obj,grad)
  }

  def fullRange = (0 until trees.length)

  def extractParser(weights: DenseVector[Double]):Builder = {
    val allWeights = partitionWeights(weights)
    assert(allWeights.length == numModels)
    val builders = Array.tabulate(numModels) { i =>
      models(i).builder(allWeights(i)):EPModel[L,W]#Builder
    }
    new Builder(builders,coarseParser,maxEPIterations)
  }


  private def expectedCounts(builder: Builder, tree: BinarizedTree[L], words: Seq[W], scorer: SpanScorer[L]) = {
    var treeScore = 0.0

    val builder.EPResult(marginals,partition,f0) = builder.buildAllCharts(words, scorer, tree)

    val expectedCounts = for( (marg,model) <- marginals zip models) yield {
      val tcounts = model.treeExpectedCounts(marg.model.asInstanceOf[model.Builder],
        tree,
        words,
        new ProjectingSpanScorer(model.projections, scorer))
      val wordCounts = new InsideOutside(marg.model.chartBuilder).expectedCounts(words, marg.inside, marg.outside, marg.partition, marg.scorer, AnchoredSpanVisitor.noOp)
      treeScore += tcounts.logProb
      tcounts -= wordCounts
    }

    (treeScore - partition,expectedCounts)
  }

  private def emptyCounts(builder: Builder) = {
    (0.0, builder.parsers.map(_.chartBuilder.grammar).map(new ExpectedCounts[W](_)))
  }

  private def sumCounts(c1: Counts, c2: Counts):Counts = {
    (c1._1 + c2._1, (c1._2 zip c2._2).map { case (a,b) => a += b})
  }

  private def countsToObjective(c: Counts) = {
    val weightVectors = for { (e,f) <- c._2 zip models if !f.locked} yield {
      expectedCountsToFeatureVector(f.indexedFeatures,e)
    }
    val grad = -tileWeightVectors( weightVectors.toIndexedSeq)
    assert(grad.data.forall(!_.isInfinite), "wtf grad")
    (-c._1,  grad)
  }

  val initialWeightVectors = models.map(_.initialWeightVector).toIndexedSeq

  def initialWeightVector = {
    val variable = (0 until numModels) collect { case m if !models(m).locked => initialWeightVectors(m)}
    tileWeightVectors(variable)
  }

  def partitionWeights(weights: DenseVector[Double]): Array[DenseVector[Double]] = {
    Array.tabulate(numModels)(m => projectWeights(weights, m))
  }

  private def tileWeightVectors(modelWeights: IndexedSeq[DenseVector[Double]]) = {
    val totalModelSize = modelWeights.map(_.size).sum
    val weights = DenseVector.zeros[Double](totalModelSize)
    var i = 0
    for(w <- modelWeights) {
      var mi = 0
      while(mi < w.size) {
        weights(i) = w(mi)
        i += 1
        mi += 1
      }
    }
    weights
  }

  private def projectWeights(weights: DenseVector[Double], modelIndex: Int) = {
    if(models(modelIndex).locked) initialWeightVectors(modelIndex)
    else {
      val result = models(modelIndex).indexedFeatures.mkDenseVector(0.0)
      for(i <- 0 until result.size) {
        result(i) = weights(i + offsets(modelIndex))
      }
      result
    }
  }

  private def expectedCountsToFeatureVector[L,W](indexedFeatures: FeatureIndexer[L,W], ecounts: ExpectedCounts[W]) = {
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
    for( (ctr, a) <- ecounts.wordCounts.zipWithIndex; (w,v) <- ctr.nonzero.pairs) {
      val vec = indexedFeatures.featuresFor(a,w)
      sumVectorIntoResults(vec, v)
    }

    result;
  }

  // more bookkeeping. offsets into giant tiled weight vector for submodels
  // don't include weights for locked features
  private val offsets = Array.fill(numModels+1)(0);
  {
    var acc = 0
    for(m <- 0 to numModels) {
      offsets(m) = acc
      if(m < numModels && !models(m).locked)
        acc += models(m).indexedFeatures.index.size
    }
  }
}

trait DiscEPModel[L,W] extends EPModel[L,W] {
  def initialWeightVector: DenseVector[Double] = {
    indexedFeatures.tabulateDenseVector(indexedFeatures.initialValueFor _)
  }

  def locked: Boolean
  def indexedFeatures: FeatureIndexer[L2,W]
  def builder(weights: DenseVector[Double]):Builder
  def treeExpectedCounts(builder: Builder,
                     tree: BinarizedTree[L],
                     w: Seq[W],
                     scorer: SpanScorer[L2]):(ExpectedCounts[W])
}

abstract class AbstractDiscEPModel[L,L3,W](proj: GrammarProjections[L,L3],
                                   root: L3,
                                   openTags: Set[L3], closedWords: Set[W]) extends DiscEPModel[L,W] {
  type L2 = L3
  def projections = proj
  def indexedFeatures:FeatureIndexer[L2,W]

  def builder(weights: DenseVector[Double]):Builder = {
    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb);
    new Builder {
      def chartBuilder = parser
    }
  }

  protected def weightsToLexicon(weights: DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(openTags, closedWords, weights, indexedFeatures);
    lexicon;
  }

  protected def weightsToGrammar(weights: DenseVector[Double]):Grammar[L2] = {
    val grammar =  FeaturizedGrammar(weights,indexedFeatures)
    grammar;
  }

}

class KMDiscEPModel[L,L3,W](proj: GrammarProjections[L,L3],
                       ann: (BinarizedTree[L],Seq[W])=>BinarizedTree[L3],
                       val indexedFeatures: FeatureIndexer[L3,W],
                       openTags: Set[L3],
                       closedWords: Set[W],
                       root: L3,
                       val locked: Boolean = false) extends AbstractDiscEPModel[L,L3,W](proj,root,openTags,closedWords) {
  def treeExpectedCounts(builder: Builder, tree: BinarizedTree[L],
                         words: Seq[W], scorer: SpanScorer[L3]) =  {
    val g = builder.chartBuilder.grammar
    val lexicon = builder.chartBuilder.lexicon
    val annotated = ann(tree,words)

    val expectedCounts = new ExpectedCounts[W](g)
    var score = 0.0;
    for(t2 <- annotated.allChildren) {
      t2 match {
        case BinaryTree(a,bt@ Tree(b,_),Tree(c,_)) =>
          val r = g.index(BinaryRule(a,b,c))
          expectedCounts.ruleCounts(r) += 1
          score += g.ruleScore(r);
        case UnaryTree(a,Tree(b,_)) =>
          val r = g.index(UnaryRule(a,b))
          expectedCounts.ruleCounts(r) += 1
          score += g.ruleScore(r);
        case n@NullaryTree(a) =>
          val aI = g.labelIndex(a)
          val w = words(n.span.start);
          expectedCounts.wordCounts(aI)(w) += 1
          score += lexicon.wordScore(g.labelIndex.get(aI), w);
      }
    }
    expectedCounts.logProb = score;
    expectedCounts
  }
}

class LatentDiscEPModel[L,L2,W](proj: GrammarProjections[L,L2],
                           featurizer: Featurizer[L2,W],
                           lexicon: Lexicon[L, W],
                           root: L2,
                           openTags: Set[L2],
                           closedWords: Set[W],
                           val locked: Boolean = false) extends AbstractDiscEPModel(proj,root,openTags,closedWords) {


  def indexedFeatures = FeatureIndexer(featurizer,lexicon,proj)

  def treeExpectedCounts(builder: Builder, tree: BinarizedTree[L], w: Seq[W], scorer: SpanScorer[L2]) = {
    new StateSplitting(builder.chartBuilder.grammar,
      builder.chartBuilder.lexicon).expectedCounts(tree.map(proj.labels.refinementsOf _),w,
      scorer)
  }

}

trait EPModelFactory[L,W] {
  def make(coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
           trainTrees: IndexedSeq[TreeInstance[L,W]],
           initLexicon: Counter2[L,W,Double],
           initBinaries: Counter2[L,BinaryRule[L],Double],
           initUnaries: Counter2[L,UnaryRule[L],Double]):DiscEPModel[L,W]
}

case class LatentDiscEPModelFactory[L,W](numStates: Int,
                                         featurizerFactory: FeaturizerFactory[L,W] = new PlainFeaturizerFactory[L](),
                                         latentFactory: LatentFeaturizerFactory = new SlavLatentFeaturizerFactory,
                                         oldWeights: File = null,
                                         locked: Boolean = false) extends EPModelFactory[L,W] {
  def make(coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
           trainTrees: IndexedSeq[TreeInstance[L,W]],
           initLexicon: Counter2[L,W,Double],
           initBinaries: Counter2[L,BinaryRule[L],Double],
           initUnaries: Counter2[L,UnaryRule[L],Double]):DiscEPModel[L,W] = {
    def split(x: L) = {
      if(x == coarseParser.root) Seq((x,0))
      else for(i <- 0 until numStates) yield (x,i);
    }
    def unsplit(l: (L,Int)) = l._1
    val indexedProjections = GrammarProjections(coarseParser.grammar, split(_:L), unsplit);

    val featurizer = featurizerFactory.getFeaturizer(initLexicon, initBinaries, initUnaries)
    val latentF = latentFactory.getFeaturizer(featurizer,numStates)

    val finalFeat: Featurizer[(L, Int), W] = if(oldWeights != null) {
      val weights = readObject[(Any,Counter[Feature[(L,Int),W],Double])](oldWeights)._2
      new CachedWeightsFeaturizer(latentF,weights,randomize=false,randomizeZeros=false)
    } else {
      latentF
    }

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t).iterator ) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1);
    }

    new LatentDiscEPModel[L,(L,Int),W](indexedProjections,
      finalFeat,
      coarseParser.lexicon,
      split(coarseParser.root)(0),
      openTags,
      closedWords)

  }

}

case class KMDiscEPModelFactory(pipeline: KMPipeline,
                                featurizerFactory: FeaturizerFactory[AnnotatedLabel,String] = new PlainFeaturizerFactory[AnnotatedLabel],
                                oldWeights: File = null,
                                locked: Boolean = false) extends EPModelFactory[String,String] {
   def make(coarseParser: ChartBuilder[LogProbabilityParseChart,String,String],
            trainTrees: IndexedSeq[TreeInstance[String,String]],
            initLexicon: Counter2[String,String,Double],
            initBinaries: Counter2[String,BinaryRule[String],Double],
            initUnaries: Counter2[String,UnaryRule[String],Double]) = {
     val transformed = trainTrees.par.map { ti =>
       val t = pipeline(ti.tree,ti.words)
       TreeInstance(ti.id,t,ti.words)
     }.seq
     val (words,binary,unary) = GenerativeParser.extractCounts(transformed);
     val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
     val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
     val proj = GrammarProjections(coarseParser.grammar,grammar,{(_:AnnotatedLabel).label})

    val featurizer = featurizerFactory.getFeaturizer(words, binary, unary);

    val finalFeat = if(oldWeights != null) {
      val weights = readObject[(Any,Counter[Feature[AnnotatedLabel,String],Double])](oldWeights)._2
      new CachedWeightsFeaturizer(featurizer,weights,randomize=false,randomizeZeros=false)
    } else {
      featurizer
    }
    val indexed = FeatureIndexer(finalFeat,lexicon,grammar)

    val openTags: Set[AnnotatedLabel] = Set.empty ++ {
      for(t <- words.nonzero.keys.iterator.map(_._1) if words(t, ::).size > 50) yield t;
    }

    val closedWords:Set[String] = Set.empty ++ {
      val wordCounts = sum(words)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 10).map(_._1);
    }

     new KMDiscEPModel[String,AnnotatedLabel,String](proj,pipeline,indexed,openTags,closedWords,transformed.head.tree.label,locked)
   }
}

object EPModelPipeline extends ParserPipeline {
  case class Params(parser: ParserParams.BaseParser[String],
                    opt: OptParams,
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 201,
                    iterPerValidate: Int = 10,
                    ep: EPPipeline.EPParams,
                    model1: EPModelFactory[String,String],
                    model2: EPModelFactory[String,String] = null,
                    model3: EPModelFactory[String,String] = null)

  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]], validate: (Parser[String, String]) => Statistics, params: Params) = {
    import params._

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val modelFactories = IndexedSeq(params.model1) ++ Option(params.model2) ++ Option(params.model3)
    println(modelFactories)
    val models = modelFactories.map(_.make(xbarParser,trainTrees,initLexicon,initBinaries,initUnaries))

    val obj = new EPModelObjective(models,trainTrees,xbarParser,params.ep.iterations)
    val cachedObj = new CachedBatchDiffFunction[DenseVector[Double]](obj);
    // new LBFGS[Int,DenseVector[Double]](iterationsPerEval,5) with ConsoleLogging;
    val init = obj.initialWeightVector + 0.0;

    def cacheWeights(weights: DenseVector[Double], iter: Int) {
      val partWeights = obj.partitionWeights(weights)
      val name = if(iter % (2*iterPerValidate) == 0) {
        "weights-a"
      } else {
        "weights-b"
      }

      for( (w,i) <- partWeights.zipWithIndex) {
        writeObject( new File(name+"-"+i+".ser"), w -> models(i).indexedFeatures.decode(w))
      }
    }

    type OptState = FirstOrderMinimizer[DenseVector[Double],BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState,Int) ) {
      val (state,iter) = pair;
      val weights = state.x;
      if(iter % iterPerValidate == 0) {
        cacheWeights(weights, iter)
        println("Validating...");
        val parser = obj.extractParser(weights);
        println(validate(parser))
      }
    }



    val it = {
      for( (state,iter) <- params.opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _);
           if iter != 0 && iter % iterationsPerEval == 0) yield try {
        val parser = obj.extractParser(state.x)
        ("EP-" + iter.toString,parser)
      } catch {
        case e => println(e);e.printStackTrace(); throw e;
      }
    }

    val pinit = obj.extractParser(init)

    Iterator("Init" -> pinit, "f0" -> pinit.f0parser) ++ it
  }
}

