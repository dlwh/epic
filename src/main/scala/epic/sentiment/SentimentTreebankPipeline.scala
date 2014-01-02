package epic.sentiment

import java.io.File
import breeze.config.CommandLineParser
import epic.trees._
import epic.parser.models.{ParserInference, ParserModel}
import epic.parser._
import breeze.linalg.{Axis, DenseVector}
import epic.framework._
import epic.constraints.{LabeledSpanConstraints, SpanConstraints, ChartConstraints}
import breeze.optimize.CachedBatchDiffFunction
import com.typesafe.scalalogging.slf4j.Logging
import epic.parser.models.SpanModelFactory
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.trees.Span
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
    testTrees.par.map{ ti =>
      val goldTree = ti.tree.children.head.map(_.label.toInt)
      val goldLabel = goldTree.label
      val goldIsNeutral = goldLabel == 2

      val marg = parser.marginal(ti.words)

      val guessTree = decode(ti.tree.map(_ => ()), marg, binary = false).map(_.label.toInt)
      val coarseTree = decode(ti.tree.map(_ => ()), marg, binary = true).map(_.label.toInt)

      val guessLabel = guessTree.label
      assert(guessTree.preorder.map(_.span).toSet == goldTree.preorder.map(_.span).toSet)
      val guess: Set[(Int, Span)] = guessTree.preorder.map(t => (t.label, t.span)).toSet
//      assert(guessTree2 == guess, (guessTree2, guess))
      val gold: Set[(Int, Span)] = goldTree.preorder.map(t => (t.label, t.span)).toSet
      assert(guess.map(_._2) == gold.map(_._2), (guess, gold, (guess--gold), (gold -- guess)))

      val coarseLabel = coarseTree.label
      val coarseGuess: Set[(Int, Span)] = coarseTree.preorder.map(t => (t.label, t.span)).toSet
      val cGuess = coarseGuess.collect { case (label, span) => (label < 2) -> span}
      val cGold: Set[(Boolean, Span)] = if(goldIsNeutral)
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

  def decode(tree: BinarizedTree[Unit], marginal: ChartMarginal[AnnotatedLabel, String], binary: Boolean) = {
    tree.extend { t =>
      val counts = marginal.marginalAt(t.begin, t.end)
      val summed = breeze.linalg.sum(counts, Axis._1)
      if(binary) {
        val neg = (summed(AnnotatedLabel("0")) + summed(AnnotatedLabel("1")) )
        val pos = (summed(AnnotatedLabel("3")) + summed(AnnotatedLabel("4")) )

        if(neg > pos) {
          AnnotatedLabel("0")
        } else  {
          AnnotatedLabel("4")
        }

      } else {
        summed.argmax
      }

    }
  }

}
