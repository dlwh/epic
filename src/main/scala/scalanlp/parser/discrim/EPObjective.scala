package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.parser.projections._
import scalanlp.trees._
import scalanlp.config.Configuration
import splitting.StateSplitting
import InsideOutside._
import java.io._;
import ParseChart.LogProbabilityParseChart;


import scalala.Scalala._;
import scalala.tensor.counters.Counters._
import scalanlp.util._;

/**
 * 
 * @author dlwh
 */
class EPObjective[L,L2,W](featurizers: Seq[Featurizer[L2,W]],
                          trees: IndexedSeq[(BinarizedTree[L],Seq[W],SpanScorer[L])],
                          indexedProjections: ProjectionIndexer[L,L2],
                          coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                          openTags: Set[L2],
                          closedWords: Set[W],
                          numModels: Int,
                          maxEPIterations: Int = 1) 
                        extends AbstractDiscriminativeObjective[L,L2,W](trees,indexedProjections,openTags,closedWords) {

  val root = {
    val splits = indexedProjections.refinementsOf(coarseParser.root)
    require(splits.length == 1)
    splits(0)
  }

  val indexedFeatures: Seq[FeatureIndexer[L2,W]] = featurizers.map { featurizer =>
    val initGrammar = coarseParser.grammar;
    val initLex = coarseParser.lexicon;
    FeatureIndexer[L,L2,W](featurizer, initGrammar, initLex, indexedProjections);
  }

  private val offsets = Array.fill(numModels+1)(0);
  {
    var acc = 0;
    for(m <- 0 to numModels) {
      offsets(m) = acc;
      if(m < numModels)
        acc += indexedFeatures(m).index.size;
    }
  }
  def totalModelSize = offsets.last;

  def extractParser(weights: DenseVector) = {
    val parsers = Array.tabulate(numModels)(extractLogProbBuilder(weights,_));
    val epBuilder = new EPParser[L,L2,W](parsers,coarseParser, Array.fill(numModels)(indexedProjections),maxEPIterations);
    epBuilder;
  }

  protected type Builder = EPParser[L,L2,W]
  protected type Counts = (Double,Seq[ExpectedCounts[W]])

  def builder(weights: DenseVector) = extractParser(weights);

  protected def emptyCounts(b: Builder) = (0.0,b.parsers.map { p => new ExpectedCounts[W](p.grammar)})
  protected def expectedCounts(epBuilder: Builder, t: BinarizedTree[L], w: Seq[W], scorer:SpanScorer[L]) = {
    val epBuilder.EPResult(charts,partition,_) = epBuilder.buildAllCharts(w,scorer,t);

    import epBuilder._;

    var treeScore = 0.0;

    val expectedCounts = for( (p,ParsedSentenceData(inside,outside,z,f0)) <- epBuilder.parsers zip charts) yield {
      val treeCounts = treeToExpectedCounts(p.grammar,p.lexicon,t,w, new ProjectingSpanScorer(indexedProjections, scorer))
      val wordCounts = wordsToExpectedCounts(p,w,inside,outside, z, f0);
      treeScore += treeCounts.logProb;
      treeCounts -= wordCounts;
    }

    (treeScore - partition,expectedCounts)
  }

  def sumCounts(c1: Counts, c2: Counts) = {
    val countsPart = Array.tabulate(numModels)(m => c1._2.apply(m) += c2._2.apply(m))
    (c1._1 + c2._1, countsPart);
  }

  def countsToObjective(c: Counts) = {
    val weightVectors = for { (e,f) <- c._2 zip indexedFeatures} yield expectedCountsToFeatureVector(f,e);
    val grad = -tileWeightVectors( weightVectors.toArray) value;
    assert(grad.forall(!_._2.isInfinite), "wtf grad");
    (-c._1,  grad);
  }


  private type LogProbBuilder = ChartBuilder[LogProbabilityParseChart,L2,W]
  private def extractLogProbBuilder(weights: DenseVector, model: Int)= {
    val grammar = weightsToGrammar(indexedFeatures(model), projectWeights(weights,model));
    val lexicon = weightsToLexicon(indexedFeatures(model), projectWeights(weights,model));
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb);
    parser
  }


  private def wordsToExpectedCounts(parser: LogProbBuilder, words: Seq[W],
                                    inside: LogProbabilityParseChart[L2],
                                    outside: LogProbabilityParseChart[L2],
                                    totalProb: Double,
                                    spanScorer: SpanScorer[L2]) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words, inside, outside, totalProb, spanScorer);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  private def treeToExpectedCounts(g: Grammar[L2],
                                   lexicon: Lexicon[L2,W],
                                   t: BinarizedTree[L],
                                   words: Seq[W],
                                   spanScorer: SpanScorer[L2]):ExpectedCounts[W] = {
    StateSplitting.expectedCounts(g,lexicon,t.map(indexedProjections.refinementsOf _),words,spanScorer);
  }

  def projectWeights(weights: DenseVector, modelIndex: Int) = {
    val result = indexedFeatures(modelIndex).mkDenseVector(0.0);
    for(i <- 0 until result.size) {
      result(i) = weights(i + offsets(modelIndex));
    }
    result;
  }

  def partitionWeights(weights: DenseVector) = {
    Array.tabulate(numModels)(m => projectWeights(weights, m));
  }

  def tileWeightVectors(modelWeights: Array[DenseVector]) = {
    val weights = new DenseVector(totalModelSize);
    var i = 0;
    for(w <- modelWeights) {
      var mi = 0;
      while(mi < w.size) {
        weights(i) = w(mi);
        i += 1;
        mi += 1;
      }
    }
    weights;
  }


  def initialWeightVector = {
    val result = new DenseVector(totalModelSize);
    var m = 0;
    for(f <- 0 until result.size) {
      if(f >= offsets(m+1)) {
        m += 1;
      }
      result(f) = indexedFeatures(m).initialValueFor(f-offsets(m));
    }
    result;
  }


}

object EPTrainer extends LatentTrainer {

  type MyFeaturizer = IndexedSeq[Featurizer[(String,Int),String]];
  type MyObjective = EPObjective[String,(String,Int),String];

  case class EPParams(models: Int = 2, iterations: Int= 1);

  case class SpecificParams(ep: EPParams);
  protected def specificManifest = manifest[SpecificParams];

  def getFeaturizer(params: Params,
                    initLexicon: PairedDoubleCounter[String, String],
                    initBinaries: PairedDoubleCounter[String, BinaryRule[String]],
                    initUnaries: PairedDoubleCounter[String, UnaryRule[String]],
                    numStates: Int): IndexedSeq[Featurizer[(String, Int), String]] = {
    val numModels = params.specific.ep.models;
    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);
    val latentFactory = params.latentFactory;
    val latentFeaturizer = latentFactory.getFeaturizer(featurizer, numStates);
    val weightsPath = params.oldWeights;
    if(weightsPath == null) {
      Array.fill(numModels)(latentFeaturizer)
    } else {
      println("Using awesome weights...");
      val weightSeq = readObject[Array[(DenseVector,DoubleCounter[Feature[(String,Int),String]])]](weightsPath).map(_._2);
      val splitFactor = params.splitFactor;
      def identity(x: Feature[(String,Int),String]) = x;
      val proj: Feature[(String,Int),String]=>Feature[(String,Int),String] = FeatureProjectors.split[String,String](_,splitFactor)

      Array.tabulate(numModels){ m =>
        if(m < weightSeq.length)
          new CachedWeightsFeaturizer(latentFeaturizer, weightSeq(m), proj)
        else latentFeaturizer;
      }
    }
  }

  def mkObjective(params: Params,
                  latentFeaturizer: MyFeaturizer,
                  trainTrees: Seq[(BinarizedTree[String], scala.Seq[String], SpanScorer[String])],
                  indexedProjections: ProjectionIndexer[String, (String, Int)],
                  xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String],
                  openTags: Set[(String, Int)],
                  closedWords: Set[String]) = {
    val maxEPIterations = params.specific.ep.iterations;
    val epModels = params.specific.ep.models;
    println(trainTrees.length)

    new EPObjective(latentFeaturizer,
      trainTrees.toIndexedSeq,
      indexedProjections,
      xbarParser,
      openTags,
      closedWords,
      epModels,
      maxEPIterations);
  }

  def cacheWeights(params: Params, obj: MyObjective, weights: DenseVector, iter: Int) = {
    val partWeights = obj.partitionWeights(weights);
    writeObject( new File("weights-"+iter +".ser"), (obj.indexedFeatures zip partWeights).map { case (f, w) => w -> f.decode(w) });
  }

}

