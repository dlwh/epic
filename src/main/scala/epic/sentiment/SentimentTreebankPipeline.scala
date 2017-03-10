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
import breeze.util.SerializableLogging
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
object SentimentTreebankPipeline extends SerializableLogging {
  case class Options(@Help(text="Treebank path")
                     path: File,
                     @Help(text="Name for the model")
                     name: String = "SentiParser",
                     opt: OptParams,
                     lossType: String = "",
                     iterationsPerEval: Int = 50,
                     @Help(text="How many iterations to run.")
                     maxIterations: Int = 202,
                     evalOnTest: Boolean = false,
                     @Help(text="Evaluate on test as well; this is so we can pick the best held-out score and evaluate that on test")
                     alsoEvalOnTest: Boolean = false,
                     includeDevInTrain: Boolean = false,
                     @Help(text="Details about the parser to build")
                     modelFactory: SpanModelFactory = new SpanModelFactory,
                     rootLossScaling: Double = 1.0,
                     computeTrainLL: Boolean = false)

  def main(args: Array[String]):Unit = {
    val params = CommandLineParser.readIn[Options](args)

    val treebank = new ProcessedTreebank(params.path, treebankType = "simple")

    var trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = treebank.trainTrees
    if (params.evalOnTest && params.includeDevInTrain)
      trainTrees ++= treebank.devTrees
    println(trainTrees.size + " train trees, " + treebank.devTrees.size + " dev trees, " + treebank.testTrees.size + " test trees")
    val gen = GenerativeParser.fromTrees(trainTrees)

    class GoldBracketingsConstraints extends ChartConstraints.Factory[AnnotatedLabel, String] {
      val trees = (trainTrees ++ treebank.devTrees ++ treebank.testTrees).map(ti => ti.words -> ti.tree).toMap
      // val trees = ((if (params.includeDevInTrain) trainTrees else trainTrees ++ treebank.devTrees) ++ treebank.testTrees).map(ti => ti.words -> ti.tree).toMap

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
      SentimentLossAugmentation.noLoss
    }
    val constrainer = new SentimentLossAugmentation(trainTrees,
      gen.topology,
      gen.lexicon,
      new GoldBracketingsConstraints,
      sentimentLoss,
      params.rootLossScaling)

    // val model = new SpanModelFactory(annotator = GenerativeParser.defaultAnnotator(vertical = params.v), dummyFeats = 0.5).make(trainTrees, constrainer)
    val model = params.modelFactory.make(trainTrees, gen.topology, gen.lexicon, new GoldBracketingsConstraints)

    val obj = new ModelObjective(model, trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val init = obj.initialWeightVector(true)

    val itr = params.opt.iterations(cachedObj, init)
    
    val name = params.name
    for ((state, iter) <- itr.take(params.maxIterations).zipWithIndex
         if iter % params.iterationsPerEval == 0) try {
      val parser = model.extractParser(state.x).copy(decoder=new MaxConstituentDecoder[AnnotatedLabel, String])
      // if (params.evalOnTest)
      //   println("Eval: " + evaluate(s"$name-$iter", parser, treebank.testTrees))
      // else
      //   println("Eval: " + evaluate(s"$name-$iter", parser, treebank.devTrees))
      if (params.computeTrainLL) {
        computeLL(trainTrees, model, state.x)
      }
      if (params.evalOnTest) {
        println("NORMAL DECODE: Eval: " + evaluate(s"$name-$iter", parser, treebank.testTrees, DecodeType.Normal))
      } else {
        println("NORMAL DECODE: Eval: " + evaluate(s"$name-$iter", parser, treebank.devTrees, DecodeType.Normal))
        if (params.alsoEvalOnTest) {
          println("TEST SET: Eval: " + evaluate(s"$name-$iter", parser, treebank.testTrees, DecodeType.Normal))
        }
      }
    } catch {
      case e: Exception => e.printStackTrace(); throw e
    }

  }
  
  def renderArr(arr: Array[Array[Int]]) = arr.map(_.map(_.toString).reduce(_ + "\t" + _)).reduce(_ + "\n" + _)

  class Model[L, W](val inner: ParserModel[L, W]) extends epic.framework.Model[TreeInstance[L, W]] {
    type ExpectedCounts = inner.ExpectedCounts
    type Marginal = inner.Marginal
    type Inference = SentimentTreebankPipeline.Inference[L, W]
    type Scorer = inner.Scorer

    def emptyCounts = inner.emptyCounts

    def accumulateCounts(inf: Inference, s: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
      inner.accumulateCounts(inf.pm.asInstanceOf[inner.Inference], s, d, m, accum, scale)
    }

    /**
     * Models have features, and this defines the mapping from indices in the weight vector to features.
     * @return
     */
    def featureIndex: Index[Feature] = inner.featureIndex

    def initialValueForFeature(f: Feature): Double = inner.initialValueForFeature(f)

    def inferenceFromWeights(weights: DenseVector[Double]): Inference = new SentimentTreebankPipeline.Inference(inner.inferenceFromWeights(weights))

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      inner.expectedCountsToObjective(ecounts)
    }
  }

  class Inference[L, W](val pm: ParserInference[L, W]) extends epic.framework.Inference[TreeInstance[L, W]] {
    val labels = pm.grammar.topology.labelIndex.toIndexedSeq.map(_ -> 0)
    type Scorer = pm.Scorer
    type Marginal = pm.Marginal

    def scorer(v: TreeInstance[L, W]): Scorer = pm.scorer(v)

    def goldMarginal(scorer: Scorer, v: TreeInstance[L, W]): Inference[L, W]#Marginal = {
      pm.goldMarginal(scorer, v)
    }

    def marginal(anch: Scorer, v: TreeInstance[L, W]): Inference[L, W]#Marginal = {
      LatentTreeMarginal[L, W](anch, v.tree.map(l => labels:scala.collection.IndexedSeq[(L, Int)]))
    }
  }

  case class Stats(spansRight: Int,
                         numSpans: Int,
                         spansRightTernary: Int, // denom is same as numSpans
                         spansRightBinary: Int,
                         numBinarySpans: Int,
                         rootsRight: Int,
                         numRoots: Int,
                         rootsRightTernary: Int, // denom is same as numSpans
                         rootsRightBinary: Int,
                         numBinaryRoots: Int) {
    def +(stats: Stats) = Stats(spansRight + stats.spansRight,
                                            numSpans + stats.numSpans,
                                            spansRightTernary + stats.spansRightTernary,
                                            spansRightBinary + stats.spansRightBinary,
                                            numBinarySpans + stats.numBinarySpans,
                                            rootsRight + stats.rootsRight,
                                            numRoots + stats.numRoots,
                                            rootsRightTernary + stats.rootsRightTernary,
                                            rootsRightBinary + stats.rootsRightBinary,
                                            numBinaryRoots + stats.numBinaryRoots)

    override def toString = {
      "Spans: " + SentimentEvaluator.renderNumerDenom(spansRight, numSpans) + " (Ternary: " + SentimentEvaluator.renderNumerDenom(spansRightTernary, numSpans) +
      "), Roots: " + SentimentEvaluator.renderNumerDenom(rootsRight, numRoots) + " (Ternary: " + SentimentEvaluator.renderNumerDenom(rootsRightTernary, numRoots) + ")"
    }

    // override def toString = f"Stats(cspans=${coarseSpansRight.toDouble/coarseSpans}%.4f: $coarseSpansRight/$coarseSpans spans=${spansRight.toDouble/numSpans}%.4f: $spansRight/$numSpans, coarseRoots=${coarseRootsRight.toDouble/numCoarseRoots}: $coarseRootsRight/$numCoarseRoots , roots=${rootsRight.toDouble/numRoots}%.4f: $rootsRight/$numRoots)"
  }

  object DecodeType extends Enumeration {
    type DecodeType = Value
    val Normal, Binary, Ternary = Value
  }
  import DecodeType._
  
  def evaluateSpanConfusions(name: String, parser: Parser[AnnotatedLabel, String], testTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], decodeType: DecodeType) = {
    testTrees.par.map { ti =>
      val spanConfusions = Array.tabulate(5, 5)((i, j) => 0)
      val goldTree = ti.tree.children.head.map(_.label.toInt)
      val marg = parser.marginal(ti.words)
      val guessTree = decode(ti.tree.map(_ => ()), marg, decodeType).map(_.label.toInt)
      val guess: Set[(Int, Span)] = guessTree.preorder.map(t => (t.label, t.span)).toSet
      val guessMap: HashMap[Span,Int] = new HashMap[Span,Int]() ++ guess.map(_.swap)
      val gold: Set[(Int, Span)] = goldTree.preorder.map(t => (t.label, t.span)).toSet
      for ((gLabel, gSpan) <- gold) {
        val pLabel = guessMap(gSpan)
        spanConfusions(gLabel)(pLabel) += 1
      }
      spanConfusions
    }.reduce((arr1, arr2) => Array.tabulate(5, 5)((i, j) => arr1(i)(j) + arr2(i)(j)))
  }
  
  def evaluateRootConfusions(name: String, parser: Parser[AnnotatedLabel, String], testTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], decodeType: DecodeType) = {
    testTrees.par.map { ti =>
      val rootConfusions = Array.tabulate(5, 5)((i, j) => 0)
      val goldTree = ti.tree.children.head.map(_.label.toInt)
      val marg = parser.marginal(ti.words)
      val guessTree = decode(ti.tree.map(_ => ()), marg, decodeType).map(_.label.toInt)
      rootConfusions(goldTree.label)(guessTree.label) += 1
      rootConfusions
    }.reduce((arr1, arr2) => Array.tabulate(5, 5)((i, j) => arr1(i)(j) + arr2(i)(j)))
  }

  def evaluate(name: String, parser: Parser[AnnotatedLabel, String], testTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], decodeType: DecodeType) = {
    println("Evaluating at " + name)
    testTrees.par.map { ti =>
      val goldTree = ti.tree.children.head.map(_.label.toInt)
      val goldRoot = goldTree.label
      val marg = parser.marginal(ti.words)
      val guessTree = decode(ti.tree.map(_ => ()), marg, decodeType).map(_.label.toInt)
      val guessRoot = guessTree.label
      val guess: Set[(Int, Span)] = guessTree.preorder.map(t => (t.label, t.span)).toSet
      val guessMap: HashMap[Span,Int] = new HashMap[Span,Int]() ++ guess.map(pair => (pair._2, pair._1))
      val gold: Set[(Int, Span)] = goldTree.preorder.map(t => (t.label, t.span)).toSet
      
      var spansRight = 0
      var numSpans = 0
      var spansRightTernary = 0
      var spansRightBinary = 0
      var numBinarySpans = 0
      for ((gLabel, gSpan) <- gold) {
        val pLabel = guessMap(gSpan)
        spansRight += (if (SentimentEvaluator.isCorrectNormal(gLabel, pLabel)) 1 else 0)
        numSpans += 1
        spansRightTernary += (if (SentimentEvaluator.isCorrectTernary(gLabel, pLabel)) 1 else 0)
        spansRightBinary += (if (SentimentEvaluator.isUsedBinaryCoarse(gLabel, pLabel) && SentimentEvaluator.isCorrectBinary(gLabel, pLabel)) 1 else 0)
        numBinarySpans += (if (SentimentEvaluator.isUsedBinaryCoarse(gLabel, pLabel)) 1 else 0)
      }
      val rootsRight = if (SentimentEvaluator.isCorrectNormal(goldRoot, guessRoot)) 1 else 0
      val numRoots = 1
      val rootsRightTernary = if (SentimentEvaluator.isCorrectTernary(goldRoot, guessRoot)) 1 else 0
      val rootsRightBinary = if (SentimentEvaluator.isUsedBinaryCoarse(goldRoot, guessRoot) && SentimentEvaluator.isCorrectBinary(goldRoot, guessRoot)) 1 else 0
      val numBinaryRoots = if (SentimentEvaluator.isUsedBinaryCoarse(goldRoot, guessRoot)) 1 else 0
      Stats(spansRight, numSpans, spansRightTernary, spansRightBinary, numBinarySpans,
                  rootsRight, numRoots, rootsRightTernary, rootsRightBinary, numBinaryRoots)
    }.reduce(_+_)
  }

  def decode(tree: BinarizedTree[Unit], marginal: ParseMarginal[AnnotatedLabel, String], decodeType: DecodeType) = {
    val (topMarg, botMarg) = marginal.labelMarginals
    tree.extend { t =>
      val summed = if (t.begin == 0 && t.end == tree.end) {
        botMarg(t.begin, t.end)
      } else {
        // Elsewhere, use the top chart
        topMarg(t.begin, t.end)
      }
      if (decodeType == Binary) {
        val neg = summed(AnnotatedLabel("0")) + summed(AnnotatedLabel("1"))
        val pos = summed(AnnotatedLabel("3")) + summed(AnnotatedLabel("4"))
        if (neg > pos) {
          AnnotatedLabel("0")
        } else  {
          AnnotatedLabel("4")
        }
      } else if (decodeType == Ternary) {
        val neg = summed(AnnotatedLabel("0")) + summed(AnnotatedLabel("1"))
        val pos = summed(AnnotatedLabel("3")) + summed(AnnotatedLabel("4"))
        val neutral = summed(AnnotatedLabel("2"))
        if(neg > pos && neg > neutral) {
          AnnotatedLabel("0")
        } else if (pos > neg && pos > neutral) {
          AnnotatedLabel("4")
        } else {
          AnnotatedLabel("2")
        }
      } else {
        summed.argmax
      }
    }
  }

  def computeLL(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], model: SpanModel[AnnotatedLabel,AnnotatedLabel,String], weights: DenseVector[Double]) {
    println("Computing final log likelihood on the whole training set...")
    val inf = model.inferenceFromWeights(weights)
    val ll = trainTrees.par.aggregate(0.0)((currLL, trainTree) => { 
      try {
        val s = inf.scorer(trainTree)
        currLL + inf.goldMarginal(s, trainTree).logPartition - inf.marginal(s, trainTree).logPartition
      } catch {
        case e: Exception => println("Couldn't parse")
        currLL
      }
    }, _ + _)
    println("Log likelihood on " + trainTrees.size + " examples: " + ll)
  }

}
