package epic.sentiment

import java.io.{FileReader, File}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.config.CommandLineParser
import epic.trees._
import epic.parser.models.{ParserInference, ParserModel, SpanModelFactory}
import epic.parser._
import breeze.linalg.DenseVector
import epic.framework._
import breeze.util.Index
import epic.constraints.{LabeledSpanConstraints, SpanConstraints, ChartConstraints}
import epic.parser.projections.ConstraintCoreGrammarAdaptor
import breeze.optimize.{BatchDiffFunction, FirstOrderMinimizer, CachedBatchDiffFunction}
import com.typesafe.scalalogging.slf4j.Logging
import epic.parser.models.SpanModelFactory
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.parser.models.SpanModelFactory
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.framework.ExpectedCounts
import epic.trees.Span
import epic.constraints.ChartConstraints.Factory
import epic.lexicon.Lexicon
import breeze.collection.mutable.TriangularArray
import breeze.numerics
import numerics.logSum
import java.util
import breeze.util._

/**
 *
 *
 * @author dlwh
 */
object SentimentTreebankPipeline extends Logging {
  case class Options(path: File, opt: OptParams, iterPerEval: Int = 100, evalOnTest: Boolean = false)


  def main(args: Array[String]):Unit = {
    val params = CommandLineParser.readIn[Options](args)

    val treebank = new ProcessedTreebank(params.path, treebankType = "simple")

    var trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = treebank.trainTrees
//    if(params.evalOnTest)
//      trainTrees ++= treebank.devTrees
    val gen = GenerativeParser.fromTrees(trainTrees)


    class GoldBracketingsConstraints extends ChartConstraints.Factory[AnnotatedLabel, String] {
      val trees = (trainTrees ++ treebank.devTrees ++ treebank.testTrees).map(ti => ti.words -> ti.tree).toMap

      def constraints(w: IndexedSeq[String]): ChartConstraints[AnnotatedLabel] = {
        val constraints = SpanConstraints.fromTree(trees.getOrElse(w, gen.bestParse(w)))
        val cons = new LabeledSpanConstraints.PromotedSpanConstraints(constraints)
        ChartConstraints(cons, cons)
      }
    }

    val constrainer = new SentimentLossAugmentation(trainTrees,
      gen.grammar,
      gen.lexicon,
      new GoldBracketingsConstraints,
      loss = SentimentLossAugmentation.defaultLoss)

    val model = new SpanModelFactory(annotator = GenerativeParser.defaultAnnotator(), dummyFeats = 0.5).make(trainTrees, constrainer)


    val obj = new ModelObjective(model, trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val init = obj.initialWeightVector(true)

    val name = "SentiParser"

    for ((state, iter) <- params.opt.iterations(cachedObj, init).take(1000).zipWithIndex
         if iter % params.iterPerEval == 0) try {
      val parser = model.extractParser(state.x).copy(decoder=new MaxConstituentDecoder[AnnotatedLabel, String])
      if(params.evalOnTest)
        println("Eval: " + evaluate(s"$name-$iter", parser, treebank.testTrees))
      else
        println("Eval: " + evaluate(s"$name-$iter", parser, treebank.devTrees))
    } catch {
      case e: Exception => e.printStackTrace(); throw e
    }


  }


  class Model[L, W](val inner: ParserModel[L, W]) extends epic.framework.Model[TreeInstance[L, W]] {
    type ExpectedCounts = inner.ExpectedCounts
    type Marginal = inner.Marginal
    type Inference = SentimentTreebankPipeline.Inference[L, W]

    def emptyCounts = inner.emptyCounts

    def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
      inner.accumulateCounts(d, m, accum, scale)
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
    val labels = pm.baseMeasure.labelIndex.toIndexedSeq.map(_ -> 0)
    type Marginal = pm.Marginal

    def goldMarginal(v: TreeInstance[L, W]): Marginal = pm.goldMarginal(v)

    def marginal(v: TreeInstance[L, W]): Marginal = {
      val anch = pm.grammar.anchor(v.words)
      LatentTreeMarginal[L, W](AugmentedAnchoring.fromRefined(anch), v.tree.map(l => labels:scala.collection.IndexedSeq[(L, Int)]))
    }
  }


  /**
 * Attempts to find a parse that maximizes the expected number
 * of correct labels. This is Goodman's MaxRecall algorithm.
 *
 * @tparam L label type
 * @tparam W word type
 */
@SerialVersionUID(2L)
  class TabooDecoder[L, W](tabooSymbols: Set[Int]) extends ChartDecoder[L, W] {

    def extractBestParse(marginal: ChartMarginal[L, W]): BinarizedTree[L] = {
      import marginal._

      val labelIndex = marginal.grammar.labelIndex

      val maxSplit = TriangularArray.fill[Int](length+1)(0)
      val maxBotLabel = TriangularArray.fill[Int](length+1)(-1)
      val maxBotScore = TriangularArray.fill[Double](length+1)(Double.NegativeInfinity)
      val maxTopLabel = TriangularArray.fill[Int](length+1)(-1)
      val maxTopScore = TriangularArray.fill[Double](length+1)(Double.NegativeInfinity)

      val scores = marginal.grammar.labelEncoder.fillArray(Double.NegativeInfinity)
      val buffer = Array.fill(1000)(Double.NegativeInfinity)

      def marginalizeRefinements(begin: Int, end: Int, l: Int, ichart: inside.ChartScores, ochart: outside.ChartScores): Double = {
        var bufOff = 0
        for (lRef <- ichart.enteredLabelRefinements(begin, end, l)) {
          val myScore = ichart.labelScore(begin, end, l, lRef) + ochart.labelScore(begin, end, l, lRef) - logPartition
          buffer(bufOff) = myScore
          bufOff += 1
          if(bufOff == buffer.length) {
            buffer(0) = breeze.numerics.logSum(buffer, buffer.length)
            bufOff = 1
          }
        }
        val sum =numerics.logSum(buffer, bufOff)
        if(tabooSymbols.contains(l)) {
          sum - 100
        } else {
          sum
        }
      }

      for(i <- 0 until inside.length) {
        util.Arrays.fill(scores, Double.NegativeInfinity)
        for(l <- inside.bot.enteredLabelIndexes(i, i + 1)) {
          scores(l) = marginalizeRefinements(i, i + 1, l, inside.bot, outside.bot)
        }
        maxBotScore(i, i + 1) = scores.max
        maxBotLabel(i, i + 1) = scores.argmax

        util.Arrays.fill(scores, Double.NegativeInfinity)
        for(l <- inside.top.enteredLabelIndexes(i, i + 1)) {
          scores(l) = marginalizeRefinements(i, i + 1, l, inside.top, outside.top)
        }
        maxTopScore(i, i + 1) = logSum(scores.max, maxBotScore(i, i + 1))
        maxTopLabel(i, i + 1) = scores.argmax
      }

      for {
        span <- 2 to inside.length
        begin <- 0 to (inside.length - span)
        end = begin + span
      } {
        util.Arrays.fill(scores, Double.NegativeInfinity)
        for(l <- inside.bot.enteredLabelIndexes(begin, end)) {
          scores(l) = marginalizeRefinements(begin, end, l, inside.bot, outside.bot)
        }
        maxBotScore(begin, end) = scores.max
        maxBotLabel(begin, end) = scores.argmax

        util.Arrays.fill(scores, Double.NegativeInfinity)
        for(l <- inside.top.enteredLabelIndexes(begin, end)) {
          scores(l) = marginalizeRefinements(begin, end, l, inside.top, outside.top)
        }
        maxTopScore(begin, end) = logSum(scores.max, maxBotScore(begin, end))
        maxTopLabel(begin, end) = scores.argmax

        val (split, splitScore) = (for(split <- begin +1 until end) yield {
          val score = logSum(maxTopScore(begin, split), maxTopScore(split, end))
          (split, score)
        }).maxBy(_._2)

        maxSplit(begin, end) = split
        maxTopScore(begin, end) = logSum(maxTopScore(begin, end), splitScore)
      }

      def bestUnaryChain(begin: Int, end: Int, bestBot: Int, bestTop: Int): IndexedSeq[String] = {
        val candidateUnaries = grammar.indexedUnaryRulesWithChild(bestBot).filter(r => grammar.parent(r) == bestTop)
        val bestChain = if (candidateUnaries.isEmpty) {
          IndexedSeq.empty
        } else if (candidateUnaries.length == 1) {
          grammar.chain(candidateUnaries(0))
        } else {
          var bestRule = candidateUnaries(0)
          var bestScore = Double.NegativeInfinity
          for (r <- candidateUnaries) {
            val aRefinements = inside.top.enteredLabelRefinements(begin, end, bestTop).toArray
            val bRefinements = inside.bot.enteredLabelRefinements(begin, end, bestBot).toArray
            val arr = new Array[Double](aRefinements.length * bRefinements.length)
            var i = 0
            for (bRef <- bRefinements; ref <- anchoring.refined.validUnaryRuleRefinementsGivenChild(begin, end, r, bRef)) {
              val aRef = anchoring.refined.parentRefinement(r, ref)
              arr(i) = (anchoring.scoreUnaryRule(begin, end, r, ref)
                + outside.top.labelScore(begin, end, bestTop, aRef)
                + inside.bot.labelScore(begin, end, bestBot, bRef)
                - logPartition
                )
              i += 1
            }
            val score = logSum(arr, i)
            if (score > bestScore) {
              bestRule = r
              bestScore = score
            }

          }
          grammar.chain(bestRule)
        }
        bestChain
      }

      def extract(begin: Int, end: Int):BinarizedTree[L] = {
        val bestBot = maxBotLabel(begin, end)
        val lower = if(begin + 1== end) {
          if(maxBotScore(begin, end) == Double.NegativeInfinity)
            throw new RuntimeException(s"Couldn't make a good score for ${(begin, end)}. InsideIndices:  ${inside.bot.enteredLabelIndexes(begin, end).toIndexedSeq}\noutside: ${outside.bot.enteredLabelIndexes(begin, end).toIndexedSeq} logPartition: $logPartition")
          NullaryTree(labelIndex.get(bestBot), Span(begin, end))
        } else {
          val split = maxSplit(begin, end)
          val left = extract(begin, split)
          val right = extract(split, end)
          BinaryTree(labelIndex.get(bestBot), left, right, Span(begin, end))
        }

        val bestTop = maxTopLabel(begin, end)
        val bestChain = bestUnaryChain(begin, end, bestBot, bestTop)

        UnaryTree(labelIndex.get(bestTop), lower, bestChain, Span(begin, end))
      }

      extract(0, inside.length)
    }
  }

  case class Stats(coarseSpansRight: Int, coarseSpans: Int,
                   spansRight: Int, numSpans: Int,
                   coarseRootsRight: Int, numCoarseRoots: Int,
                   rootsRight: Int, numRoots: Int) {
    def +(stats: Stats) = Stats(
    coarseSpansRight + stats.coarseSpansRight, coarseSpans + stats.coarseSpans,
      spansRight + stats.spansRight, numSpans + stats.numSpans,
      coarseRootsRight + stats.coarseRootsRight, numCoarseRoots + stats.numCoarseRoots,
      rootsRight + stats.rootsRight, numRoots + stats.numRoots )

    override def toString = f"Stats(cspans=${coarseSpansRight.toDouble/coarseSpans}%.4f: $coarseSpansRight/$coarseSpans spans=${spansRight.toDouble/numSpans}%.4f: $spansRight/$numSpans, coarseRoots=${coarseRootsRight.toDouble/numCoarseRoots}: $coarseRootsRight/$numCoarseRoots , roots=${rootsRight.toDouble/numRoots}%.4f: $rootsRight/$numRoots)"
  }


  def evaluate(name: String, parser: Parser[AnnotatedLabel, String], testTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val two = parser.grammar.labelIndex(AnnotatedLabel("2"))
    assert(two >= 0)
    val coarseLabelParser = parser.copy(decoder= new TabooDecoder[AnnotatedLabel, String](Set(two)))

    testTrees.par.map{ ti =>
      val goldTree = ti.tree.children.head.map(_.label.toInt)
      val goldLabel = goldTree.label
      val goldIsNeutral = goldLabel == 2

      val ps = AnnotatedLabelChainReplacer.replaceUnaries(parser.bestParse(ti.words))
      val coarseSent = AnnotatedLabelChainReplacer.replaceUnaries(coarseLabelParser.bestParse(ti.words))
      assert(ps.children.length == 1)
      val guessTree = ps.children.head.map(_.label.toInt)
      val guessLabel = guessTree.label
      assert(guessTree.preorder.map(_.span).toSet == goldTree.preorder.map(_.span).toSet)
      val guess = guessTree.preorder.drop(1).map(t => (t.label, t.span)).toSet
      val gold = goldTree.preorder.drop(1).map(t => (t.label, t.span)).toSet
      assert(guess.size == gold.size)

      val coarseTree = coarseSent.children.head.map(_.label.toInt)
      val coarseLabel = coarseTree.label
      val coarseGuess = coarseTree.preorder.drop(1).map(t => (t.label, t.span)).toSet
      val cGuess = coarseGuess.collect { case (label, span) => (label < 2) -> span}
      val cGold = if(goldIsNeutral)
        Set.empty[(Boolean, Span)]
      else
        gold.collect { case (label, span) if label != 2 => (label < 2) -> span}

      assert(coarseLabel != 2, coarseTree.render(ti.words))

      val coarseRootRight = if (goldLabel match {
        case 0 | 1 => coarseLabel < 2
        case 2 => false
        case 3 | 4 => coarseLabel >= 2
      })
        1
      else
        0

      val stats = Stats(cGuess & cGold size, cGold.size,
        guess & gold size, gold.size,
        coarseRootRight, if(goldIsNeutral) 0 else 1,
        if(guessLabel == goldLabel) 1 else 0, 1)
      println(s"Guess\n ${guessTree.render(ti.words)}\nGold\n${goldTree.render(ti.words)}\n$stats")
      stats
    }.reduce(_ + _)
  }

}
