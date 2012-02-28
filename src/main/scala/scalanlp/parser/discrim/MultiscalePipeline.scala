package scalanlp.parser.discrim

import scalanlp.util._
import scalanlp.parser._
import java.io._
import projections.{ProjectingSpanScorer, GrammarProjections}
import scalala.library.Library
import scalanlp.parser.ParseChart._
import scalala.library.Library._
import scalala.tensor.{Counter, Counter2}
import scalala.tensor.::
import logging._
import scalanlp.optimize.{BatchDiffFunction, FirstOrderMinimizer, CachedBatchDiffFunction}
import scalala.tensor.dense.{DenseVectorCol, DenseVector}
import scalanlp.parser.InsideOutside.ExpectedCounts
import scalanlp.stats.distributions.Rand
import splitting.MultiscaleStateSplitting

/**
 * Runs something not unlike Petrov's 2008 EMNLP paper.
 *
 * @author dlwh
 */
object MultiscalePipeline extends ParserPipeline {

  type MyFeaturizer = Featurizer[(String,Seq[Int]),String]
  type MyObjective = MultiscaleObjective[String,(String,Seq[Int]),String]

  case class SpecificParams(lastParser: File = null)
  implicit def specificManifest = manifest[SpecificParams]

  type Params = LatentParams[SpecificParams];
  protected lazy val paramManifest = { manifest[Params]}

  def getFeaturizer(params: MultiscalePipeline.Params,
                    initLexicon: Counter2[String, String, Double],
                    initBinaries: Counter2[String, BinaryRule[String], Double],
                    initUnaries: Counter2[String, UnaryRule[String], Double],
                    hierarchy: MultiscaleHierarchy[(String,Seq[Int])]) = {
    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);
    val latentFeaturizer = new MultiscaleFeaturizer(featurizer, hierarchy)
    val weightsPath = params.oldWeights;
    println("old weights: " + weightsPath);
    if(weightsPath == null) {
      latentFeaturizer
    } else {
      val weights = readObject[(DenseVector[Double],Counter[Feature[(String,Seq[Int]),String],Double])](weightsPath)._2;
      println(weights.size,weights.valuesIterator.count(_ == 0.0),weights.valuesIterator.count(_.abs < 1E-4))

      new CachedWeightsFeaturizer(latentFeaturizer, weights, randomize= false, randomizeZeros = true);
    }
  }

  def determineInitialRules(xbarGrammar: Grammar[String],  params: Params) = {
    if(params.specific.lastParser == null) {
      val result = Counter[Rule[(String,Seq[Int])],Double]()
      for( p@(_,rule) <- xbarGrammar.binaryRules.nonzero.keys) {
        val splitRule = BinaryRule(rule.parent -> Seq.empty, rule.left -> Seq.empty, rule.right -> Seq.empty)
        result(splitRule) = xbarGrammar.binaryRules(p)
      }

      for( p@(_,rule) <- xbarGrammar.unaryRules.nonzero.keys) {
        val splitRule = UnaryRule(rule.parent -> Seq.empty, rule.child -> Seq.empty)
        result(splitRule) = xbarGrammar.unaryRules(p)
      }

      result

    } else {
      val oldParser = readObject[SimpleChartParser[String,(String,Seq[Int]),String]](params.specific.lastParser)
      val result = Counter[Rule[(String,Seq[Int])],Double]()
      for( p@(_,rule) <- oldParser.builder.grammar.binaryRules.keysIterator) {
        result(rule) = oldParser.builder.grammar.binaryRules(p)
      }

      for( p@(_,rule) <- oldParser.builder.grammar.unaryRules.nonzero.keys) {
        result(rule) = oldParser.builder.grammar.unaryRules(p)
      }

      result
    }

  }

  def splitRules(rules: Counter[Rule[(String,Seq[Int])],Double], splitAllRules: Boolean = false) = {
    val splitter = new ConditionalRuleSplitter(fracToSplit = if(splitAllRules) 1.0 else 0.5, minRuleValue = if(splitAllRules) -1 else 1E-4)
    def split(sym: (String,Seq[Int])) = if(sym._1 == "") Seq(sym._1 -> Seq(0)) else Seq((sym._1,0 +: sym._2), (sym._1,1 +: sym._2))
    def proj(sym: (String,Seq[Int])) = sym._1
    splitter.chooseRulesToSplit(rules, split, proj)
  }

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);
    import params._;

    val xbarParser = parser.optParser.getOrElse {
      val grammar: Grammar[String] = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    val initialRules = determineInitialRules(xbarParser.grammar, params)

    val (splitSymbols,theSplitRules) = splitRules(initialRules, splitAllRules = params.specific.lastParser == null)
    val allSplitSymbols = splitSymbols.mapValues(_.toIndexedSeq)
    println(allSplitSymbols)
    val splitGrammar = {
      val b = scalala.tensor.mutable.Counter2[(String,Seq[Int]),BinaryRule[(String,Seq[Int])],Double]()
      val u = scalala.tensor.mutable.Counter2[(String,Seq[Int]),UnaryRule[(String,Seq[Int])],Double]()
      for( r <- theSplitRules) r match {
        case r@BinaryRule(_,_,_) => b(r.parent,r) = 1
        case r@UnaryRule(_,_) => u(r.parent,r) = 1
      }
      Grammar(b,u)
    }


    def unsplit(s: (String,Any)) = s._1
    def split(s: String) = allSplitSymbols(s)

    val indexedProjections = GrammarProjections(xbarParser.grammar, splitGrammar, unsplit _ );
    val hierarchy = MultiscaleHierarchy.make(indexedProjections.labels.fineIndex, {(fine: (String,Seq[Int])) =>
      if(fine._2.isEmpty) None
      else {
        var f = (fine._1,fine._2.drop(1))
        var i = indexedProjections.labels.fineIndex(f)
        while(i == -1 && !f._2.isEmpty) {
          f = (f._1,f._2.drop(1))
          i = indexedProjections.labels.fineIndex(f)
        }
        if(i == -1) None
        else Some(f)
      }
    })

    val latentFeaturizer: MyFeaturizer = getFeaturizer(params, initLexicon, initBinaries, initUnaries, hierarchy)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t).iterator if hierarchy.isFinestLevel(t2)) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1);
    }

    val obj = new MultiscaleObjective(latentFeaturizer,
      trainTrees,
      indexedProjections,
      hierarchy,
      xbarParser,
      openTags,
      closedWords) with ConfiguredLogging;

    val init = obj.initialWeightVector + 0.0;

    type OptState = FirstOrderMinimizer[DenseVector[Double],BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState,Int) ) {
      val (state,iter) = pair;
      val weights = state.x;
      if(iter % iterPerValidate == 0) {
        cacheWeights(params, obj,weights, iter);
        println("Validating...");
        val parser = obj.extractParser(weights);
        println(validate(parser))
      }
    }


    val cachedObj = new CachedBatchDiffFunction[DenseVector[Double]](obj);

    for( (state,iter) <- params.opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _);
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractParser(state.x)
      ("MultiScale-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }
  }


  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector[Double], iter: Int) = {
    println("Zeros:" + weights.size,weights.valuesIterator.count(_ == 0), weights.valuesIterator.count(_.abs < 1E-4))
    writeObject( new File("weights-"+iter +".ser"), weights -> obj.indexedFeatures.decode(weights))
  }
}

class MultiscaleObjective[L,L2,W](featurizer: Featurizer[L2,W],
                            trees: IndexedSeq[TreeInstance[L,W]],
                            indexedProjections: GrammarProjections[L,L2],
                            hierarchy: MultiscaleHierarchy[L2],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                            openTags: Set[L2],
                            closedWords: Set[W],
                            specificSpans: Seq[SpanScorer[L2]] = Stream.continually(SpanScorer.identity[L2]),
                            weightedBroker: WeightedSpanBrokerFactory[L,W] = WeightedSpanBrokerFactory.identity[L,W]
                            ) extends AbstractDiscriminativeObjective[L,L2,W](trees,indexedProjections,openTags,closedWords, specificSpans) {


  /** The split root symbol */
  val root = {
    val splits = indexedProjections.labels.refinementsOf(coarseParser.root)
    require(splits.length == 1, splits)
    splits(0)
  }

  /**
   * For each production, its features
   */
  val indexedFeatures: FeatureIndexer[L2,W] = {
    val initLex = coarseParser.lexicon
    FeatureIndexer[L,L2,W](featurizer, initLex, indexedProjections)
  }
  val numSpanWeights = weightedBroker.numWeights(indexedProjections.labels.fineIndex, indexedProjections.rules.fineIndex)
  println("Num features: " + indexedFeatures.index.size)
  println("Num span features: " + numSpanWeights)

  /**
   * Returns a parser for a set of weights.
   * Weights are linearized
   */
  def extractParser(weights: DenseVector[Double]) = {
    // XXX fit in the span broker
    val parser = new SimpleChartParser[L,L2,W](builder(weights).builder,new MaxConstituentDecoder(indexedProjections),indexedProjections)
    parser
  }


  /**
   * A parser in the max-semiring
   */
  def extractMaxParser(weights: DenseVector[Double]) = {
    val parser = new SimpleChartParser[L,L2,W](builder(weights).builder.withCharts(ParseChart.viterbi),
      new ViterbiDecoder(indexedProjections.labels),indexedProjections)
    parser
  }

  protected case class Builder(builder: MultiscaleChartBuilder[LogProbabilityParseChart,L2,W],
                               broker: BaseWeightedSpanBroker[L,L2,W])
  protected type Counts = (ExpectedCounts[W],DenseVectorCol[Double])

  protected def builder(weights: DenseVector[Double])= {
    val grammarWeights:DenseVectorCol[Double] = weights.asCol(0 until indexedFeatures.index.size) + 0.0
    val grammar = weightsToGrammar(indexedFeatures, grammarWeights)
    val lexicon = weightsToLexicon(indexedFeatures, grammarWeights)
    val parser = new MultiscaleChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, hierarchy, ParseChart.logProb)

    val otherWeights = if(numSpanWeights == 0) DenseVector.zeros[Double](0) else weights.asCol(indexedFeatures.index.size until (indexedFeatures.index.size + numSpanWeights)) + 0.0
    val actualBroker = weightedBroker.broker(grammar.labelIndex, grammar.index, otherWeights)
    Builder(parser,actualBroker)
  }

  protected def emptyCounts(b: Builder) = new ExpectedCounts[W](b.builder.grammar) -> DenseVector.zeros[Double](numSpanWeights)
  protected def expectedCounts(b: Builder, ti: TreeInstance[L,W], specific: SpanScorer[L2]) = {
    val summed = SpanScorer.sum(specific,new ProjectingSpanScorer(indexedProjections, ti.spanScorer))
    val treeCounts = treeToExpectedCounts(b.builder.grammar, b.builder.lexicon, b.broker, ti, summed)
    val wordCounts = wordsToExpectedCounts(ti, b, summed)
    /*
    println(w)
    println(t render w)
    println("tree:" + treeCounts.logProb + " " + treeCounts.decode(b.grammar))
    println("words:" + wordCounts.logProb + " " + wordCounts.decode(b.grammar))
    if(treeCounts.logProb - wordCounts.logProb > 1E-4) error(t.render(w) + " " + treeCounts + " " + wordCounts)
    */
    val r = (treeCounts._1 -= wordCounts._1, treeCounts._2 -= wordCounts._2)
    if(r._1.logProb.isNaN || r._1.logProb.isInfinite) {
      println("Warning: NaN's in ecounts for" + ti.words)
      emptyCounts(b)
    } else {
      r
    }
  }

  def sumCounts(c1: Counts, c2: Counts) = { (c1._1 += c2._1, c1._2 += c2._2) }

  def countsToObjective(c: Counts) = {
    val counts = expectedCountsToFeatureVector(indexedFeatures, c._1)
    val grad = if(c._2.size == 0) counts else DenseVector.vertcat(counts,c._2)
    val obj = -c._1.logProb
    (obj,-grad)
  }

  def wordsToExpectedCounts(ti: TreeInstance[L,W],
                            parser: Builder,
                            spanScorer: SpanScorer[L2] = SpanScorer.identity):Counts = {
    val composite = SpanScorer.sum(spanScorer,parser.broker.spanForId(ti.id))
    val spanCounts = DenseVector.zeros[Double](numSpanWeights)
    val visitor = parser.broker.ecountsVisitor(ti.id,spanCounts.data)
    val ecounts = new InsideOutside(parser.builder).expectedCounts(ti.words,composite,visitor)
    ecounts -> spanCounts
  }

  // these expected counts are in normal space, not log space.
  def treeToExpectedCounts(g: Grammar[L2],
                           lexicon: Lexicon[L2,W],
                           broker: BaseWeightedSpanBroker[L,L2,W],
                           ti: TreeInstance[L,W],
                           spanScorer: SpanScorer[L2] = SpanScorer.identity) = {
    val composite = SpanScorer.sum(spanScorer,broker.spanForId(ti.id))
    val spanCounts = DenseVector.zeros[Double](numSpanWeights)
    val visitor = broker.ecountsVisitor(ti.id,spanCounts.data)
    val ecounts = new MultiscaleStateSplitting(g,lexicon,hierarchy).expectedCounts(ti.tree.map(indexedProjections.labels.refinementsOf _),ti.words,
      composite,spanVisitor=visitor)

    ecounts -> spanCounts

  }

  def initialWeightVector = {
    val result = DenseVector.zeros[Double](indexedFeatures.index.size + numSpanWeights)
    for(f <- 0 until indexedFeatures.index.size) {
      result(f) = indexedFeatures.initialValueFor(f)
    }

    for(f <- indexedFeatures.index.size until result.size) {
      result(f) = -Rand.uniform.get()/10
    }
    result
  }
}