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

import epic.parser.projections.{AnchoredSpanProjector, ChartProjector, AnchoredPCFGProjector, AnchoredRuleMarginalProjector}
import epic.trees._
import breeze.collection.mutable.TriangularArray
import breeze.util.SerializableLogging
import breeze.linalg.argmax

trait ParserException extends Exception
case class ParseExtractionException(msg: String, sentence: IndexedSeq[Any]) extends RuntimeException(msg) with ParserException
case class NoParseException(msg: String, sentence: IndexedSeq[Any], cause: Throwable = null) extends RuntimeException(s"No parse for $sentence: $msg") with ParserException

/**
 * A ChartDecoder converts marginals into a binarized tree. Post-processing
 * to debinarize and strip useless annotations is still necessary.
 *
 * @author dlwh
 */
trait ChartDecoder[L, W] extends Serializable{
  def extractBestParse(marginal: ParseMarginal[L, W]):BinarizedTree[L]
  def wantsMaxMarginal: Boolean = false
}

object ChartDecoder {
  def apply[L,W]():ChartDecoder[L, W] = {
    new MaxRuleProductDecoder()
  }
}

/**
 * Tries to extract a tree that maximizes log score.
 *
 * @author dlwh
 */
@SerialVersionUID(2)
case class ViterbiDecoder[L, W]() extends ChartDecoder[L, W] with Serializable with SerializableLogging {

  override def wantsMaxMarginal: Boolean = true

  override def extractBestParse(marginal: ParseMarginal[L, W]): BinarizedTree[L] = {
    extractMaxDerivationParse(marginal).map(_._1)
  }

  def extractMaxDerivationParse(marginal: ParseMarginal[L, W]): BinarizedTree[(L, Int)] = {
    assert(marginal.isMaxMarginal, "Viterbi only makes sense for max marginal marginals!")
    import marginal._
    val labelIndex = topology.labelIndex
    val rootIndex = topology.rootIndex
    val refined = anchoring

    def buildTreeUnary(begin: Int, end:Int, root: Int, rootRef: Int):BinarizedTree[(L, Int)] = {
      var maxScore = Double.NegativeInfinity
      var maxChild = -1
      var maxChildRef = -1
      var maxRule = -1
      for {
        r <- topology.indexedUnaryRulesWithParent(root)
        refR <- refined.validRuleRefinementsGivenParent(begin, end, r, rootRef)
      } {
        val ruleScore = anchoring.scoreUnaryRule(begin, end, r, refR)
        val b = topology.child(r)
        val refB = refined.childRefinement(r, refR)
        val score = ruleScore + insideBotScore(begin, end, b, refB)
        if (score > maxScore) {
          maxScore = score
          maxChild = b
          maxChildRef = refB
          maxRule = r
        }
      }

      if (maxScore == Double.NegativeInfinity) {
        throw new ParseExtractionException(s"Couldn't find a tree! [$begin,$end) ${topology.labelIndex.get(root)}", words)
      }

      val child = buildTree(begin, end, maxChild, maxChildRef)
      UnaryTree(labelIndex.get(root) ->rootRef, child, topology.chain(maxRule), Span(begin, end))
    }

    def buildTree(begin: Int, end: Int, root: Int, rootRef: Int):BinarizedTree[(L, Int)] = {
      var maxScore = Double.NegativeInfinity
      var maxLeft = -1
      var maxRight = -1
      var maxLeftRef = -1
      var maxRightRef = -1
      var maxSplit = -1
      var maxRule = -1

      if (begin + 1 == end) {
        return NullaryTree(labelIndex.get(root) -> rootRef, Span(begin, end))
      }

      val spanScore = anchoring.scoreSpan(begin, end, root, rootRef)
      for {
        r <- topology.indexedBinaryRulesWithParent(root)
        b = topology.leftChild(r)
        c = topology.rightChild(r)
        refR <- refined.validRuleRefinementsGivenParent(begin, end, r, rootRef)
        refB = refined.leftChildRefinement(r, refR)
        refC = refined.rightChildRefinement(r, refR)
        split <- marginal.feasibleSplitPoints(begin, end, b, refB, c, refC)
      } {
        val ruleScore = anchoring.scoreBinaryRule(begin, split, end, r, refR)
        val score = (
          ruleScore
            + marginal.insideTopScore(begin, split, b, refB)
            + marginal.insideTopScore(split, end, c, refC)
            + spanScore
          )
        if (score > maxScore) {
          maxScore = score
          maxLeft = b
          maxLeftRef = refB
          maxRight = c
          maxRightRef = refC
          maxSplit = split
          maxRule = r
        }
      }

      if (maxScore == Double.NegativeInfinity) {
        throw new ParseExtractionException(s"Couldn't find a tree! [$begin,$end) ${topology.labelIndex.get(root)}\n", marginal.words)
      } else {
        val lchild = buildTreeUnary(begin, maxSplit, maxLeft, maxLeftRef)
        val rchild = buildTreeUnary(maxSplit, end, maxRight, maxRightRef)
        BinaryTree(labelIndex.get(root) -> rootRef, lchild, rchild, Span(begin, end))
      }
    }

    val maxRootRef = refined.validLabelRefinements(0, length, rootIndex).maxBy(ref => insideTopScore(0, length, rootIndex, ref))
    val t = buildTreeUnary(0, length, rootIndex, maxRootRef)
    t
  }
}

abstract class ProjectingChartDecoder[L, W](proj: ChartProjector[L, W]) extends ChartDecoder[L, W] {
  def extractBestParse(marginal: ParseMarginal[L, W]): BinarizedTree[L] = {
    val anchoring = proj.project(marginal)
    val newMarg = anchoring.maxMarginal
    assert(!newMarg.logPartition.isInfinite, marginal.logPartition + " " + newMarg.logPartition)
    new ViterbiDecoder[L, W].extractBestParse(newMarg)
  }
}

/**
 * Tries to extract a tree that maximizes rule product in the coarse grammar.
 * This is Slav's Max-Rule-Product
 *
 * @author dlwh
 */
@SerialVersionUID(3904281411822753993L)
case class MaxRuleProductDecoder[L, W]() extends ProjectingChartDecoder[L, W](new AnchoredRuleMarginalProjector())

/**
 * Projects a tree to an anchored PCFG and then does viterbi on that tree.
 * This is the Max-Variational method in Matsuzaki
 *
 * @author dlwh
 */
case class MaxVariationalDecoder[L, W]() extends ProjectingChartDecoder[L, W](new AnchoredPCFGProjector())

/**
 * Attempts to find a parse that maximizes the expected number
 * of correct labels. This is Goodman's MaxRecall algorithm.
 *
 * @tparam L label type
 * @tparam W word type
 */
@SerialVersionUID(2L)
class MaxConstituentDecoder[L, W] extends ChartDecoder[L, W] {

  def extractBestParse(marginal: ParseMarginal[L, W]): BinarizedTree[L] = {
    val length = marginal.length
    import marginal.topology

    val spanMarginals = new AnchoredSpanProjector().projectSpanPosteriors(marginal)
    val maxSplit = TriangularArray.fill[Int](length+1)(0)
    val maxBotLabel = TriangularArray.fill[Int](length+1)(-1)
    val maxBotScore = TriangularArray.fill[Double](length+1)(0.0)
    val maxTopLabel = TriangularArray.fill[Int](length+1)(-1)
    val maxTopScore = TriangularArray.fill[Double](length+1)(0.0)

    val numLabels = topology.labelIndex.size

    for {
      span <- 1 to length
      begin <- 0 to (length - span)
      end = begin + span
    } {
      maxBotLabel(begin, end) = argmax(spanMarginals.botType(begin, end).slice(0, numLabels))
      maxBotScore(begin, end) = spanMarginals.botType(begin, end)(maxBotLabel(begin, end))

      maxTopLabel(begin, end) = argmax(spanMarginals.topType(begin, end).slice(0, numLabels))
      maxTopScore(begin, end) = spanMarginals.botType(begin, end)(maxBotLabel(begin, end)) + maxBotScore(begin, end)

      if (end - begin > 1) {
        val (split, splitScore) = (for (split <- begin + 1 until end) yield {
          val score = maxTopScore(begin, split) + maxTopScore(split, end)
          (split, score)
        }).maxBy(_._2)

        maxSplit(begin, end) = split
        maxTopScore(begin, end) = maxTopScore(begin, end) + splitScore
      }
    }

    def bestUnaryChain(begin: Int, end: Int, bestBot: Int, bestTop: Int): IndexedSeq[String] = {
      val candidateUnaries = topology.indexedUnaryRulesWithChild(bestBot).filter(r => topology.parent(r) == bestTop)
      val bestChain = if (candidateUnaries.isEmpty) {
        IndexedSeq.empty
      } else if (candidateUnaries.length == 1) {
        topology.chain(candidateUnaries(0))
      } else {
        var bestRule = candidateUnaries(0)
        // TODO: restore this!
//        var bestScore = Double.NegativeInfinity
//        for (r <- candidateUnaries) {
//          val aRefinements = inside.top.enteredLabelRefinements(begin, end, bestTop).toArray
//          val bRefinements = inside.bot.enteredLabelRefinements(begin, end, bestBot).toArray
//          var score = 0.0
//          for (bRef <- bRefinements; ref <- anchoring.validUnaryRuleRefinementsGivenChild(begin, end, r, bRef)) {
//            val aRef = anchoring.parentRefinement(r, ref)
//            score+= math.exp(anchoring.scoreUnaryRule(begin, end, r, ref)
//              + outside.top.labelScore(begin, end, bestTop, aRef)
//              + inside.bot.labelScore(begin, end, bestBot, bRef)
//              - logPartition
//              )
//          }
//          if (score > bestScore) {
//            bestRule = r
//            bestScore = score
//          }
//
//        }
        topology.chain(bestRule)
      }
      bestChain
    }

    def extract(begin: Int, end: Int):BinarizedTree[L] = {
      val bestBot = maxBotLabel(begin, end)
      val lower = if (begin + 1== end) {
      // if (maxBotScore(begin, end) == Double.NegativeInfinity)
      //  throw new RuntimeException(s"Couldn't make a good score for ${(begin, end)}. InsideIndices:  ${inside.bot.enteredLabelIndexes(begin, end).toIndexedSeq}\noutside: ${outside.bot.enteredLabelIndexes(begin, end).toIndexedSeq} logPartition: $logPartition")
        NullaryTree(topology.labelIndex.get(bestBot), Span(begin, end))
      } else {
        val split = maxSplit(begin, end)
        val left = extract(begin, split)
        val right = extract(split, end)
        BinaryTree(topology.labelIndex.get(bestBot), left, right, Span(begin, end))
      }

      val bestTop = maxTopLabel(begin, end)
      val bestChain = bestUnaryChain(begin, end, bestBot, bestTop)

      UnaryTree(topology.labelIndex.get(bestTop), lower, bestChain, Span(begin, end))
    }

    extract(0, length)
  }
}
