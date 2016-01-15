package epic.sentiment

import java.io.File
import breeze.config.Help
import breeze.config.CommandLineParser
import epic.trees._
import epic.parser.models._
import epic.parser._
import breeze.linalg._
import epic.framework._
import epic.constraints.{LabeledSpanConstraints, SpanConstraints, ChartConstraints}
import breeze.optimize._
import breeze.optimize.FirstOrderMinimizer.OptParams
import com.typesafe.scalalogging.slf4j.LazyLogging
import epic.parser.models.SpanModelFactory
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.trees.Span
import scala.collection.mutable.HashMap
import breeze.util._
import epic.parser.models.ParserExtractableModelFactory
import epic.dense.AdadeltaGradientDescentDVD

/**
 *
 *
 * @author dlwh
 */
object NeuralSentimentTreebankPipeline extends LazyLogging {
  case class Options(@Help(text="Treebank path")
                     path: File,
                     @Help(text="Name for the model")
                     name: String = null,
                     opt: OptParams,
                     lossType: String = "",
                     iterationsPerEval: Int = 100,
                     @Help(text="How many iterations to run.")
                     maxIterations: Int = 1002,
                     evalOnTest: Boolean = false,
                     includeDevInTrain: Boolean = false,
                     @Help(text="Details about the parser to build")
                     modelFactory: PositionalNeuralModelFactory = new PositionalNeuralModelFactory,
                     rootLossScaling: Double = 1.0,
                     threads: Int = -1,
                     @Help(text="Scale of random weight initialization")
                     initWeightsScale: Double = 1E-2,
                     @Help(text="String to specify fancier initialization types based on fan-in/fan-out")
                     initializerSpec: String = "",
                     @Help(text="True if we should determinimize training (remove randomness associated with random minibatches)")
                     determinizeTraining: Boolean = false,
                     @Help(text="Use Adadelta for optimiziation instead of Adagrad")
                     useAdadelta: Boolean = true,
                     momentum: Double = 0.95)


  def main(args: Array[String]):Unit = {
    val params = CommandLineParser.readIn[Options](args)

    val treebank = new ProcessedTreebank(params.path, treebankType = "simple")

    var trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = treebank.trainTrees
    println("First training tree: " + trainTrees.head.toString)
    if(params.evalOnTest && params.includeDevInTrain)
      trainTrees ++= treebank.devTrees
    println(trainTrees.size + " train trees, " + treebank.devTrees.size + " dev trees, " + treebank.testTrees.size + " test trees");
    val gen = GenerativeParser.fromTrees(trainTrees)


    class GoldBracketingsConstraints extends ChartConstraints.Factory[AnnotatedLabel, String] {
      val trees = (trainTrees ++ treebank.devTrees ++ treebank.testTrees).map(ti => ti.words -> ti.tree).toMap
//      val trees = ((if (params.includeDevInTrain) trainTrees else trainTrees ++ treebank.devTrees) ++ treebank.testTrees).map(ti => ti.words -> ti.tree).toMap

      def constraints(w: IndexedSeq[String]): ChartConstraints[AnnotatedLabel] = {
        val constraints = SpanConstraints.fromTree(trees.getOrElse(w, gen.bestBinarizedTree(w)))
        val cons = new LabeledSpanConstraints.PromotedSpanConstraints(constraints)
        ChartConstraints(cons, cons)
      }
    }

    // TODO: params are inelegant
    val sentimentLoss: (Int, Int) => Double = if (params.lossType == "defaultLoss") {
      SentimentLossAugmentation.defaultLoss
    } else if (params.lossType == "posNegLoss") {
      SentimentLossAugmentation.posNegLoss
    } else if (params.lossType == "hammingLoss") {
      SentimentLossAugmentation.hammingLoss
    } else {
      SentimentLossAugmentation.noLoss;
    }
    val constrainer = new SentimentLossAugmentation(trainTrees,
      gen.topology,
      gen.lexicon,
      new GoldBracketingsConstraints,
      sentimentLoss,
      params.rootLossScaling)

//    val model = new SpanModelFactory(annotator = GenerativeParser.defaultAnnotator(vertical = params.v), dummyFeats = 0.5).make(trainTrees, constrainer)
    val model = params.modelFactory.make(trainTrees, gen.topology, gen.lexicon, new GoldBracketingsConstraints)

    val obj = new ModelObjective(model, trainTrees, params.threads)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val init = model.initialWeightVector(params.initWeightsScale, params.initializerSpec)

    val name = Option(params.name).orElse(Option(model.getClass.getSimpleName).filter(_.nonEmpty)).getOrElse("DiscrimParser")

//    val itr = params.opt.iterations(cachedObj, init)
    val itr: Iterator[FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State] = if (params.determinizeTraining) {
      val scanningBatchesObj = cachedObj.withScanningBatches(params.opt.batchSize)
      if (params.useAdadelta) {
        println("OPTIMIZATION: Adadelta")
        new AdadeltaGradientDescentDVD(params.opt.maxIterations, params.momentum).iterations(scanningBatchesObj, init).
            asInstanceOf[Iterator[FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State]]
      } else {
        println("OPTIMIZATION: Adagrad")
        params.opt.iterations(scanningBatchesObj, init).asInstanceOf[Iterator[FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State]]
      }
    } else {
      if (params.useAdadelta) {
        println("OPTIMIZATION: Adadelta")
        new AdadeltaGradientDescentDVD(params.opt.maxIterations, params.momentum).iterations(cachedObj.withRandomBatches(params.opt.batchSize), init).
            asInstanceOf[Iterator[FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State]]
      } else {
        println("OPTIMIZATION: Adagrad")
        params.opt.iterations(cachedObj, init)
      }
    }
    
    
    for ((state, iter) <- itr.take(params.maxIterations).zipWithIndex
         if iter % params.iterationsPerEval == 0) try {
      val parser = model.extractParser(state.x).copy(decoder=new MaxConstituentDecoder[AnnotatedLabel, String])
//      if(params.evalOnTest)
//        println("Eval: " + evaluate(s"$name-$iter", parser, treebank.testTrees))
//      else
//        println("Eval: " + evaluate(s"$name-$iter", parser, treebank.devTrees))
      if(params.evalOnTest) {
        println("NORMAL DECODE: Eval: " + SentimentTreebankPipeline.evaluate(s"$name-$iter", parser, treebank.testTrees, SentimentTreebankPipeline.DecodeType.Normal));
      } else {
//        println("Span confusions");
//        println(renderArr(evaluateSpanConfusions(s"$name-$iter", parser, treebank.devTrees, DecodeType.Normal)));
//        println("Root confusions");
//        println(renderArr(evaluateRootConfusions(s"$name-$iter", parser, treebank.devTrees, DecodeType.Normal)));
        println("NORMAL DECODE: Eval: " + SentimentTreebankPipeline.evaluate(s"$name-$iter", parser, treebank.devTrees, SentimentTreebankPipeline.DecodeType.Normal));
        // 10/24/2015: Ternary decode seems like what we were doing when we submitted the paper the first time?
//        println("TERNARY DECODE: Eval: " + evaluate(s"$name-$iter", parser, treebank.devTrees, DecodeType.Ternary));
//        println("BINARY DECODE: Eval: " + evaluateBetter(s"$name-$iter", parser, treebank.devTrees, DecodeType.Binary));
      }
    } catch {
      case e: Exception => e.printStackTrace(); throw e
    }


  }
}
