package scalanlp.ontonotes

import scalanlp.parser.ParseEval.Statistics
import scalanlp.parser.Grammar._
import scalala.library.Library
import scalanlp.parser._

import discrim._
import projections.{ProjectingSpanScorer, GrammarProjections}
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
import scalanlp.trees._
import scalala.tensor.dense.DenseVector
import scalanlp.util._
import scalanlp.optimize.{BatchDiffFunction, FirstOrderMinimizer, RandomizedGradientCheckingFunction, CachedBatchDiffFunction}

object DiscrimOntoPipeline extends BasicOntoPipeline {

  protected val paramManifest = manifest[Params];
  case class Params(parser: ParserParams.BaseParser[OntoLabel],
                    opt: OptParams,
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 201,
                    iterPerValidate: Int = 10,
                    pipeline: KMPipeline,
                    oldWeights: File = null)


  def trainParser(trainTrees: IndexedSeq[TreeInstance[OntoLabel, String]], validate: (Parser[OntoLabel, String]) => Statistics, params: Params) = {
    val strippedTrees = for (ti <- trainTrees) yield {
      new TreeInstance(ti.id, ti.tree.map(stripAnnotations _), ti.words)
    }
    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(strippedTrees);

    import params._

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,OntoLabel,String](OntoLabel("TOP"),lexicon,grammar,ParseChart.logProb);
    }

    val modelFactories = IndexedSeq( new KMOntoDiscEPModelFactory(new KMPipeline()), new NEREPModelFactory[String]())
    println(modelFactories)
    val models = modelFactories.map(_.make(xbarParser,trainTrees,initLexicon,initBinaries,initUnaries))

    val obj = new EPModelObjective(models,trainTrees,xbarParser,4)
    val cachedObj = new CachedBatchDiffFunction[DenseVector[Double]](obj);
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

    it
  }
}