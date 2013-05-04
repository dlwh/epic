package epic.parser
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import java.util.Arrays
import projections.{AnchoredPCFGProjector, AnchoredRuleMarginalProjector}
import epic.trees._
import breeze.collection.mutable.TriangularArray
import breeze.util._
import breeze.numerics._
import epic.lexicon.Lexicon


/**
 * A ChartDecoder converts marginals into a binarized tree. Post-processing
 * to debinarize and strip useless annotations is still necessary.
 *
 * @author dlwh
 */
trait ChartDecoder[L, W] extends Serializable{
  def extractBestParse(marginal: ChartMarginal[L, W]):BinarizedTree[L]
}

object ChartDecoder extends Serializable {
  def apply[L,W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]):ChartDecoder[L, W] = {
    new MaxRuleProductDecoder(grammar, lexicon)
  }
}

/**
 * Tries to extract a tree that maximizes log score.
 *
 * @author dlwh
 */
@SerialVersionUID(2)
class ViterbiDecoder[L, W] extends ChartDecoder[L, W] with Serializable {

  override def extractBestParse(marginal: ChartMarginal[L, W]): BinarizedTree[L] = {
    import marginal._
    val labelIndex = grammar.labelIndex
    val rootIndex = (grammar.labelIndex(grammar.root))
    val refined = anchoring.refined

    def buildTreeUnary(begin: Int, end:Int, root: Int, rootRef: Int):BinarizedTree[L] = {
      var maxScore = Double.NegativeInfinity
      var maxChild = -1
      var maxChildRef = -1
      var maxRule = -1
      for {
        r <- grammar.indexedUnaryRulesWithParent(root)
        refR <- refined.validRuleRefinementsGivenParent(begin, end, r, rootRef)
      } {
        val ruleScore = anchoring.scoreUnaryRule(begin, end, r, refR)
        val b = grammar.child(r)
        val refB = refined.childRefinement(r, refR)
        val score = ruleScore + inside.bot(begin, end, b, refB)
        if(score > maxScore) {
          maxScore = score
          maxChild = b
          maxChildRef = refB
          maxRule = r
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(begin, end).map { case (i, v) => (grammar.labelIndex.get(i), v)}.toList)
        sys.error(s"Couldn't find a tree! [$begin,$end) $grammar.labelIndex.get(root)")
      }
      val child = buildTree(begin, end, maxChild, maxChildRef)
      UnaryTree(labelIndex.get(root), child, grammar.chain(maxRule), Span(begin, end))
    }

    def buildTree(begin: Int, end: Int, root: Int, rootRef: Int):BinarizedTree[L] = {
      var maxScore = Double.NegativeInfinity
      var maxLeft = -1
      var maxRight = -1
      var maxLeftRef = -1
      var maxRightRef = -1
      var maxSplit = -1
      var maxRule = -1
      if(begin +1 == end) {
        return NullaryTree(labelIndex.get(root), Span(begin, end))
      }

      val spanScore = anchoring.scoreSpan(begin, end, root, rootRef)
      for {
        r <- grammar.indexedBinaryRulesWithParent(root)
        b = grammar.leftChild(r)
        c = grammar.rightChild(r)
        refR <- refined.validRuleRefinementsGivenParent(begin, end, r, rootRef)
        refB = refined.leftChildRefinement(r, refR)
        refC = refined.rightChildRefinement(r, refR)
        split <- inside.top.feasibleSpan(begin, end, b, refB, c, refC)
      } {
        val ruleScore = anchoring.scoreBinaryRule(begin, split, end, r, refR)
        val score = (
          ruleScore
            + inside.top.labelScore(begin, split, b, refB)
            + inside.top.labelScore(split, end, c, refC)
            + spanScore
          )
        if(score > maxScore) {
          maxScore = score
          maxLeft = b
          maxLeftRef = refB
          maxRight = c
          maxRightRef = refC
          maxSplit = split
          maxRule = r
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(begin, end).map { case (i, v) => (grammar.labelIndex.get(i), v)}.toList)
        sys.error(s"Couldn't find a tree! [$begin,$end) $grammar.labelIndex.get(root)")
      } else {
        val lchild = buildTreeUnary(begin, maxSplit, maxLeft, maxLeftRef)
        val rchild = buildTreeUnary(maxSplit, end, maxRight, maxRightRef)
        BinaryTree(labelIndex.get(root), lchild, rchild, Span(begin, end))
      }


    }

    val maxRootRef = refined.validLabelRefinements(0, inside.length, rootIndex).maxBy(ref => inside.top(0, inside.length, rootIndex, ref))
    val t = buildTreeUnary(0, inside.length, rootIndex, maxRootRef)
    t
  }
}

/**
 * Tries to extract a tree that maximizes rule product in the coarse grammar.
 * This is Slav's Max-Rule-Product
 *
 * @author dlwh
 */
case class MaxRuleProductDecoder[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]) extends ChartDecoder[L, W] {
  private val p = new AnchoredRuleMarginalProjector[L,W](-7)

  def extractBestParse(marginal: ChartMarginal[L, W]): BinarizedTree[L] = {
    val anchoring = p.project(marginal)
    val newMarg = anchoring.marginal
    new MaxConstituentDecoder[L, W].extractBestParse(newMarg)
  }
}

/**
 * Projects a tree to an anchored PCFG and then does viterbi on that tree.
 * This is the Max-Variational method in Matsuzaki
 *
 * @author dlwh
 */
class MaxVariationalDecoder[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]) extends ChartDecoder[L, W] {
  private val p = new AnchoredPCFGProjector[L,W](grammar)

  def extractBestParse(marginal: ChartMarginal[L, W]): BinarizedTree[L] = {
    val anchoring = p.project(marginal)
    val newMarg = anchoring.marginal
    new MaxConstituentDecoder[L, W].extractBestParse(newMarg)
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
class MaxConstituentDecoder[L, W] extends ChartDecoder[L, W] {

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
        if(bufOff == 1000) {
          buffer(0) = inside.sum(buffer, buffer.length)
          bufOff = 1
        }
      }
      inside.sum(buffer, bufOff)
    }

    for(i <- 0 until inside.length) {
      Arrays.fill(scores, Double.NegativeInfinity)
      for(l <- inside.bot.enteredLabelIndexes(i, i + 1)) {
        scores(l) = marginalizeRefinements(i, i + 1, l, inside.bot, outside.bot)
      }
      maxBotScore(i, i + 1) = scores.max
      maxBotLabel(i, i + 1) = scores.argmax

      Arrays.fill(scores, Double.NegativeInfinity)
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
      Arrays.fill(scores, Double.NegativeInfinity)
      for(l <- inside.bot.enteredLabelIndexes(begin, end)) {
        scores(l) = marginalizeRefinements(begin, end, l, inside.bot, outside.bot)
      }
      maxBotScore(begin, end) = scores.max
      maxBotLabel(begin, end) = scores.argmax

      Arrays.fill(scores, Double.NegativeInfinity)
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

    def bestUnaryChain(begin: Int, end: Int, bestBot: Int, bestTop: Int): Seq[String] = {
      val candidateUnaries = grammar.indexedUnaryRulesWithChild(bestBot).filter(r => grammar.parent(r) == bestTop)
      val bestChain = if (candidateUnaries.isEmpty) {
        Seq.empty
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
          throw new RuntimeException(s"Couldn't make a good score for ${(begin, end)}. InsideIndices:  ${inside.bot.enteredLabelIndexes(begin, end).toIndexedSeq}\noutside: ${outside.bot.enteredLabelIndexes(begin, end).toIndexedSeq}")
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
