package scalanlp.ontonotes

import scalanlp.parser.ParseEval.Statistics
import scalanlp.parser.Grammar._
import scalala.library.Library
import scalanlp.parser._

import discrim.{DiscrimObjective, FeaturizerFactory, PlainFeaturizerFactory}
import scalanlp.optimize.FirstOrderMinimizer._
import scalanlp.parser.ParserParams
import java.io.File
import scalanlp.parser.TreeInstance
import scalanlp.parser.Parser
import scalanlp.parser.GenerativeParser
import scalala.tensor.::
import scalala.library.Library
import scalanlp.parser.SimpleLexicon
import scalanlp.parser.CKYChartBuilder
import scalanlp.parser.ParseChart._
import scalanlp.parser.ParseChart
import scalala.library.Library._
import scalanlp.util.logging.ConfiguredLogging
import scalanlp.optimize.RandomizedGradientCheckingFunction
import scalanlp.optimize.CachedBatchDiffFunction

object DiscrimOntoPipeline extends BasicOntoPipeline {

  protected val paramManifest = manifest[Params];
  case class Params(parser: ParserParams.BaseParser[OntoLabel],
                    opt: OptParams,
                    featurizerFactory: FeaturizerFactory[OntoLabel,String] = new PlainFeaturizerFactory[OntoLabel],
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 201,
                    iterPerValidate: Int = 10,
                    oldWeights: File = null)



  def trainParser(trainTrees2: IndexedSeq[TreeInstance[OntoLabel, String]], validate: (Parser[OntoLabel, String]) => Statistics, params: Params) = {
    val trainTrees = for( ti <- trainTrees2) yield ti.copy(ti.id, ti.tree.map(stripAnnotations _))
    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,OntoLabel,String](OntoLabel("TOP"),lexicon,grammar,ParseChart.logProb);
    }

    import params._;

    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.iterator.map(_._1) if initLexicon(t, ::).size > 50) yield t;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 10).map(_._1);
    }

    val obj = new DiscrimObjective(featurizer, trainTrees, xbarParser, openTags, closedWords) with ConfiguredLogging;

    // new LBFGS[Int,DenseVector[Double]](iterationsPerEval,5) with ConsoleLogging;
    val init = obj.initialWeightVector;
    val rand = new RandomizedGradientCheckingFunction(obj, 0.1);

    for( (state,iter) <- params.opt.iterations(new CachedBatchDiffFunction(obj),init).take(maxIterations).zipWithIndex;
         if iter != 0 && iter % iterationsPerEval == 0) yield {
       val parser = obj.extractParser(state.x);
       (iter + "", parser);
    }

  }
}