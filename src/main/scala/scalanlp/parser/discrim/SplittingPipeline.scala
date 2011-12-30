package scalanlp.parser.discrim

import scalanlp.util._
import scalanlp.parser._
import java.io._
import projections.{ConstraintScorer, ProjectingSpanScorer, ProjectionIndexer, GrammarProjections}
import scalala.tensor.dense.DenseVector
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalala.library.Library
import scalanlp.parser.ParseChart._
import scalala.library.Library._
import scalanlp.optimize.CachedBatchDiffFunction
import scalala.tensor.{Counter, Counter2}
import scalala.tensor.::
import logging._
import scalanlp.collection.mutable.TriangularArray

/**
 * Runs a parser that can conditionally split labels
 *
 * @author dlwh
 */
object SplittingPipeline extends ParserPipeline {

  type Sym = (String,Seq[Int])
  type MyFeaturizer = Featurizer[Sym,String]
  type MyObjective = LatentDiscrimObjective[String,Sym,String]

  case class SpecificParams(lastParser: File = null, fracToSplit:Double = 0.5, refinedSpans: SpanParams[Sym]) {
  }
  implicit def specificManifest = manifest[SpecificParams]

  type Params = LatentParams[SpecificParams];
  protected lazy val paramManifest = { manifest[Params]}

  def mkObjective(params: SplittingPipeline.Params,
                  latentFeaturizer: SplittingPipeline.MyFeaturizer,
                  trainTrees: IndexedSeq[TreeInstance[String, String]],
                  indexedProjections: GrammarProjections[String, (String, Seq[Int])],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Seq[Int])],
                  closedWords: Set[String],
                  oneStepProjections: ProjectionIndexer[Sym,Sym]) = {
    class ProjectScorer(scorer: SpanScorer[Sym]) extends SpanScorer[Sym] {
      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

      def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

      def scoreSpan(begin: Int, end: Int, tag: Int) = {
        scorer.scoreSpan(begin,end,oneStepProjections.project(tag))
      }
    }
    val broker = params.specific.refinedSpans.trainSpans
    val spans = trainTrees.map(instance => new ProjectScorer(broker.spanForId(instance.id)))
    val r = new LatentDiscrimObjective(latentFeaturizer,
      trainTrees,
      indexedProjections,
      xbarParser,
      openTags,
      closedWords, spans) with ConfiguredLogging;

    r
  }


  def getFeaturizer(params: SplittingPipeline.Params,
                    initLexicon: Counter2[String, String, Double],
                    initBinaries: Counter2[String, BinaryRule[String], Double],
                    initUnaries: Counter2[String, UnaryRule[String], Double],
                    numStates: Int) = {
    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);
    val latentFeaturizer = new MultiscaleFeaturizer(featurizer)
    val weightsPath = params.oldWeights;
    println("old weights: " + weightsPath);
    if(weightsPath == null) {
      latentFeaturizer
    } else {
      val weights = readObject[(DenseVector[Double],Counter[Feature[(String,Seq[Int]),String],Double])](weightsPath)._2;
      println(weights.size,weights.valuesIterator.count(_ == 0.0),weights.valuesIterator.count(_.abs < 1E-4))
      def projectFeature(f: Feature[Sym,String]) = f match {
        case SubstateFeature(k,states) if params.splitFactor > 1 => SubstateFeature(k,states.map(_.dropRight(1)))
        case _ => f
      }

      new CachedWeightsFeaturizer(latentFeaturizer, weights, randomize= false, randomizeZeros = true);
    }
  }

  def split(sym: Sym) = if(sym._1 == "") Seq(sym._1 -> Seq(0)) else Seq((sym._1,sym._2 :+ 0), (sym._1,sym._2 :+ 1))
  def initialSplit(syms: Index[String]) = {
    def make(sym: String) = sym -> Seq.empty[Int]
    val init = Index(syms.map(make))
    ProjectionIndexer.fromSplitter(init, split)
  }

  def splitLabels(oneStepProjections: ProjectionIndexer[Sym,Sym],
                  builder: SimpleChartParser[String,Sym,String],
                  trees: IndexedSeq[TreeInstance[String,String]], fracToSplit: Double = 0.5) = {
    val splitter = new ConditionalLabelSplitter(oneStepProjections, split, fracToSplit)
    val splitTrees = trees.map { i =>
      i.copy(tree = i.tree.map(l => builder.projections.labels.refinementsOf(l).toSeq), spanScorer = SpanScorer.identity)
    }
    splitter.splitLabels(builder.builder,splitTrees)
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

    val (oneStepProjections,siblingProjections) = if(specific.lastParser == null) {
      val r = initialSplit(xbarParser.grammar.labelIndex)
      r -> r
    } else {
      val oldParser = readObject[SimpleChartParser[String,Sym,String]](params.specific.lastParser)
      val lastProjectionsFile = new File(params.specific.lastParser.getParentFile.getParentFile,"projections.ser")
      val lastSiblings = readObject[ProjectionIndexer[Sym,Sym]](lastProjectionsFile)
      val r = splitLabels(lastSiblings, oldParser, trainTrees, params.specific.fracToSplit)
      println("Old parser projections:")
      println(oldParser.projections.labels.fineIndex)
      r
    }
    writeObject(new File("projections.ser"), siblingProjections)
    println("Siblings:")
    println(siblingProjections.coarseIndex)
    println("One step:")
    println(oneStepProjections.coarseIndex)
    println(oneStepProjections.fineIndex)
    println("Span Index:")
    println(params.specific.refinedSpans.index)

    def unsplit(s: (String,Any)) = s._1
    val labelProjections = ProjectionIndexer(xbarParser.grammar.labelIndex, oneStepProjections.fineIndex, unsplit)
    def split(s: String) = labelProjections.refinementsOf(s)

    val indexedProjections = {
      val raw = GrammarProjections(xbarParser.grammar, split _, unsplit _ )
      GrammarProjections(labelProjections, raw.rules)
    };

    val latentFeaturizer: MyFeaturizer = getFeaturizer(params, initLexicon, initBinaries, initUnaries, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t).iterator ) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1);
    }

    val obj = mkObjective(params, latentFeaturizer, trainTrees, indexedProjections, xbarParser, openTags, closedWords, oneStepProjections)

    val optimizer = opt.minimizer(obj);

    val init = obj.initialWeightVector + 0.0;

    import scalanlp.optimize.RandomizedGradientCheckingFunction;
    val rand = new RandomizedGradientCheckingFunction(obj,1E-4);
    def evalAndCache(pair: (optimizer.State,Int) ) {
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

    for( (state,iter) <- optimizer.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _);
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractParser(state.x)
      ("MultiScale-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }
  }


  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector[Double], iter: Int) = {
    println("Zeros:" + weights.size,weights.valuesIterator.count(_ == 0), weights.valuesIterator.count(_.abs < 1E-4))
    val name = if(iter/10 % 2 == 0) "weights-b.ser" else "weights-a.ser"
    writeObject( new File(name), weights -> obj.indexedFeatures.decode(weights))
  }

}