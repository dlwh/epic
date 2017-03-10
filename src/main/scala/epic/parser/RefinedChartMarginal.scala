package epic.parser

import breeze.collection.mutable.TriangularArray
import breeze.linalg.{ Counter2, max, softmax }
import breeze.util.SerializableLogging
import epic.trees.{ BinarizedTree, Span, UnaryTree }

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

/**
 * Holds the information for the marginals for a sentence.
 * That is, the inside and outside scores for a sentence
 * and anchoring.
 *
 * @param anchoring the specialized grammar used to construct the marginals for this sentence
 * @param inside inside chart
 * @param outside outside chart
 * @param logPartition the normalization constant aka inside score of the root aka probability of the sentence
 * @tparam L the label type
  * @tparam W the word type
  */
final case class RefinedChartMarginal[L, W](
    anchoring: GrammarAnchoring[L, W],
    inside: RefinedParseChart[L], outside: RefinedParseChart[L],
    logPartition: Double,
    override val isMaxMarginal: Boolean
) extends ParseMarginal[L, W] with SerializableLogging {

  override def insideTopScore(begin: Int, end: Int, sym: Int, ref: Int): Double = inside.top(begin, end, sym, ref)
  override def insideBotScore(begin: Int, end: Int, sym: Int, ref: Int): Double = inside.bot(begin, end, sym, ref)

  def marginalAt(begin: Int, end: Int):Counter2[L, Int, Double] = {
    val in = inside.bot.decodedLabelScores(begin, end)
    in += outside.bot.decodedLabelScores(begin, end)
    in -= logPartition
    breeze.numerics.exp(in)
  }

  def feasibleSplitPoints(begin: Int, end: Int, leftChild: Int, leftChildRef: Int, rightChild: Int, rightChildRef: Int):IndexedSeq[Int] = {
    inside.top.feasibleSplitPoints(begin, end, leftChild, leftChildRef, rightChild, rightChildRef).toIndexedSeq
  }

  /**
   * Forest traversal that visits spans in a "bottom up" order.
   */
  def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double = Double.NegativeInfinity):Unit = {
    if (logPartition.isInfinite) throw new RuntimeException("No parse for " + words)
    if (logPartition.isNaN) throw new RuntimeException("NaN prob!")

    val itop = inside.top

    val lexLoc = anchoring.tagConstraints

    // handle lexical
    for (i <- words.indices) {
      var visitedSomething  = false
      for {
        a <- lexLoc.allowedTags(i) if  anchoring.sparsityPattern.bot.isAllowedLabeledSpan(i, i+1, a)
        ref <- anchoring.validLabelRefinements(i, i+ 1, a)
      } {
        val score:Double = anchoring.scoreSpan(i, i+1, a, ref) + outside.bot(i, i+1, a, ref) - logPartition
        assert(!score.isNaN, anchoring.scoreSpan(i, i+1, a, ref) + " " + outside.bot(i, i+1, a, ref) + " "+ logPartition)
        if (score != Double.NegativeInfinity) {
          spanVisitor.visitSpan(i, i+1, a, ref, math.exp(score))
          visitedSomething = true
        }
      }
      assert(visitedSomething, lexLoc.allowedTags(i).toIndexedSeq + " " + logPartition + " " + anchoring.words)
    }

    // handle binaries
    for {
      span <- 2 to inside.length
      begin <- 0 to (inside.length - span)
    } {
      val end:Int = begin + span

      for (a <- inside.bot.enteredLabelIndexes(begin, end); refA <- inside.bot.enteredLabelRefinements(begin, end, a)) {
        var i = 0
        val aOutside = outside.bot.labelScore(begin, end, a, refA)
        val labelMarginal = aOutside + inside.bot.labelScore(begin, end, a, refA) - logPartition
        val aScore = aOutside + anchoring.scoreSpan(begin, end, a, refA)
        if (labelMarginal > spanThreshold) {
          spanVisitor.visitSpan(begin, end, a, refA, math.exp(labelMarginal))
          if (!spanVisitor.skipBinaryRules) {

            val rules = anchoring.validCoarseRulesGivenParentRefinement(a, refA)
            while (i < rules.length) {
              val r = rules(i)
              val b = topology.leftChild(r)
              val c = topology.rightChild(r)
              i += 1

              val feasibleCoarseRange = inside.top.feasibleSplitPoints(begin, end, b, c)

              if (feasibleCoarseRange.nonEmpty) {
                val refinements = anchoring.validRuleRefinementsGivenParent(begin, end, r, refA)
                var ruleRefIndex = 0
                while (ruleRefIndex < refinements.length) {
                  val refR = refinements(ruleRefIndex)
                  ruleRefIndex += 1
                  val refB = anchoring.leftChildRefinement(r, refR)
                  val refC = anchoring.rightChildRefinement(r, refR)

                  val feasibleSplitRange = inside.top.feasibleSplitPoints(begin, end, b, refB, c, refC)

                  var split = feasibleSplitRange.begin
                  val endSplit = feasibleSplitRange.end

                  while (split < endSplit) {
                    val bInside = itop.labelScore(begin, split, b, refB)
                    val cInside = itop.labelScore(split, end, c, refC)
                    val withoutRefined = bInside + cInside
                    if (!java.lang.Double.isInfinite(withoutRefined)) {
                      val ruleScore = anchoring.scoreBinaryRule(begin, split, end, r, refR)
                      val score = aScore + withoutRefined + ruleScore - logPartition
                      val expScore = math.exp(score)
                      spanVisitor.visitBinaryRule(begin, split, end, r, refR, expScore)
                    }

                    split += 1
                  }
                }
              }
            }
          }
        }
      }
    }

    // Unaries
    if (!spanVisitor.skipUnaryRules)
      for {
        span <- 1 to words.length
        begin <- 0 to (words.length - span)
        end = begin + span
        a <- inside.top.enteredLabelIndexes(begin, end)
        refA <- inside.top.enteredLabelRefinements(begin, end, a)
      } {
        val aScore = outside.top.labelScore(begin, end, a, refA)
        for (r <- topology.indexedUnaryRulesWithParent(a); refR <- anchoring.validRuleRefinementsGivenParent(begin, end, r, refA)) {
          val b = topology.child(r)
          val refB = anchoring.childRefinement(r, refR)
          val bScore = inside.bot.labelScore(begin, end, b, refB)
          val rScore = anchoring.scoreUnaryRule(begin, end, r, refR)
          val prob = math.exp(bScore + aScore + rScore - logPartition)
          if (prob > 0)
            spanVisitor.visitUnaryRule(begin, end, r, refR, prob)
        }
      }
  }


  def checkForTree(tree: BinarizedTree[(L, Int)]) = {
    for (t <- tree.allChildren) t match {
      case UnaryTree( (label, ref), _, _, span) =>
        val labelScore = inside.top(span.begin, span.end, anchoring.topology.labelIndex(label), ref)
        if (labelScore.isInfinite) {
          logger.warn("problem with unary: " + (label, ref) + " " + span)
        }
      case tree =>
        val labelScore = inside.bot(tree.span.begin, tree.span.end, anchoring.topology.labelIndex(tree.label._1), tree.label._2)
        if (labelScore.isInfinite) {
          logger.warn("problem with other: " + t.label + " " + tree.span)
        }
    }
    this
  }

  def checkForSimpleTree(tree: BinarizedTree[L]) = {
    for (t <- tree.allChildren) t match {
      case UnaryTree( label, _, _, span) =>
        val labelScore = breeze.linalg.softmax(inside.top.decodedLabelScores(span.begin, span.end, anchoring.topology.labelIndex(label)))
        if (labelScore.isInfinite) {
          logger.warn("problem with unary: " + label + " " + span)
        }
      case tree =>
        val labelScore = breeze.linalg.softmax(inside.bot.decodedLabelScores(tree.begin, tree.end, anchoring.topology.labelIndex(tree.label)))
        if (labelScore.isInfinite) {
          logger.warn("problem with other: " + t.label + " " + tree.span)
        }
    }
    this
  }

  def checkForTreeOutside(tree: BinarizedTree[(L, Int)]) {
    for (t <- tree.allChildren) t match {
      case tree@UnaryTree( (label, ref), _, _, span) =>
        val ai: Int = anchoring.topology.labelIndex(label)
        val labelScore = outside.top(span.begin, span.end, ai, ref)
        if (labelScore.isInfinite) {
          RefinedChartMarginal.synchronized {
            logger.warn("problem with top: " + (label, ref) + " " + span)
            logger.warn(s"problem with outside other: ${t.label} ${tree.span} ${outside.top.enteredLabelIndexes(tree.span.begin, tree.span.end)(ai)} $words ${outside.top.decodedLabelScores(tree.span.begin,tree.span.end)}")
            logger.warn(ai + " " + outside.top.enteredLabels(TriangularArray.index(tree.span.begin, tree.span.end)))
            logger.warn("Constraint: " + anchoring.sparsityPattern.top.isAllowedLabeledSpan(tree.begin, tree.end, ai))
            logger.warn("checking for inside starting from here...")
            checkForTree(t.asInstanceOf[BinarizedTree[(L, Int)]])
            logger.warn("done.")
          }
          return
        }
      case tree =>
        val ai: Int = anchoring.topology.labelIndex(tree.label._1)
        val labelScore = outside.bot(tree.span.begin, tree.span.end, ai, tree.label._2)
        if (labelScore.isInfinite) {
          RefinedChartMarginal.synchronized {
            logger.warn(s"problem with outside other: ${t.label} ${tree.span} ${outside.bot.enteredLabelIndexes(tree.span.begin, tree.span.end)(ai)} $words ${outside.bot.decodedLabelScores(tree.span.begin,tree.span.end)}")
            logger.warn(ai + " " + outside.bot.enteredLabels(TriangularArray.index(tree.span.begin, tree.span.end)))
            logger.warn("Constraint: " + anchoring.sparsityPattern.bot.isAllowedLabeledSpan(tree.begin, tree.end, ai))
            logger.warn("checking for inside starting from here...")
            checkForTree(t.asInstanceOf[BinarizedTree[(L, Int)]])
          }
          return
        }
    }
  }

}

object RefinedChartMarginal {

  def apply[L, W](grammar: Grammar[L, W], sent: IndexedSeq[W]): RefinedChartMarginal[L, W] = {
    apply(grammar.anchor(sent))
  }

  def apply[L, W](anchoring: GrammarAnchoring[L, W]): RefinedChartMarginal[L, W] = {
    apply(anchoring, false)
  }

  def apply[L, W](anchoring: GrammarAnchoring[L, W], maxMarginal: Boolean): RefinedChartMarginal[L, W] = {
    val sent = anchoring.words
    val sum = if (maxMarginal) MaxSummer else LogSummer
    val inside = buildInsideChart(anchoring, sent, sum)
    val logPartition = rootScore(anchoring, inside, sum)
    val outside = buildOutsideChart(anchoring, inside, sum)
    RefinedChartMarginal(anchoring, inside, outside, logPartition, maxMarginal)
  }

  private[parser] trait Summer {
    def apply(a: Double, b: Double):Double
    def apply(a: Array[Double], length: Int):Double
  }

  private[parser] object LogSummer extends Summer {
    def apply(a: Double, b: Double): Double = softmax(a,b)
    def apply(a: Array[Double], length: Int): Double = softmax.array(a, length)
  }

  private[parser] object MaxSummer extends Summer {
    def apply(a: Double, b: Double): Double = math.max(a,b)
    def apply(a: Array[Double], length: Int): Double = if (length == 0) Double.NegativeInfinity else max.array(a, length)
  }

  private def rootScore[L, W](anchoring: GrammarAnchoring[L, W], inside: RefinedParseChart[L], sum: Summer): Double = {
    val rootIndex: Int = anchoring.topology.labelIndex(anchoring.topology.root)
    val rootScores = new Array[Double](anchoring.validLabelRefinements(0, inside.length, rootIndex).length)
    var offset = 0
    for(ref <- inside.top.enteredLabelRefinements(0, inside.length, rootIndex)) {
      val score = inside.top.labelScore(0, inside.length, rootIndex, ref)
      if (score != Double.NegativeInfinity) {
        rootScores(offset) = score
        offset += 1
      }
    }
    val score = sum(rootScores, offset)
    // assert(score != 0.0, rootScores.mkString(", ") + anchoring.words)
    assert(!score.isNaN, rootScores.mkString(", "))
    score
  }

  // first parse chart is the inside scores, second parse chart is span scores for spans that were computed.
  private def buildInsideChart[L, W](anchoring: GrammarAnchoring[L, W],
                                     words: IndexedSeq[W], sum: Summer): RefinedParseChart[L] = {

    val inside = RefinedParseChart(anchoring.topology.labelIndex,
      Array.tabulate(anchoring.topology.labelIndex.size)(anchoring.numValidRefinements),
      words.length,
      anchoring.sparsityPattern)
    val tagConstraints = anchoring.tagConstraints

    // handle lexical
    for{i <- anchoring.words.indices} {
      assert(anchoring.sparsityPattern.isAllowedSpan(i,i+1), "a pos tag isn't allowed? " + anchoring.sparsityPattern)
      assert(anchoring.sparsityPattern.bot.isAllowedSpan(i,i+1), "a top of a length 1 span isn't allowed?")
      var foundSomething = false
      for {
        a <- tagConstraints.allowedTags(i) if anchoring.sparsityPattern.bot.isAllowedLabeledSpan(i, i+1, a)
        ref <- anchoring.validLabelRefinements(i, i+1, a)
      } {
        val score:Double = anchoring.scoreSpan(i, i+1, a, ref)
        if (score != Double.NegativeInfinity) {
          inside.bot.enter(i, i+1, a, ref, score)
          foundSomething = true
        }
      }

      updateInsideUnaries(inside, anchoring,  i, i+1, sum)
    }


    val scoreArray = Array.ofDim[Double](anchoring.maxLabelRefinements,  40)
    val offsets = new Array[Int](anchoring.maxLabelRefinements)

    // a -> bc over [begin, split, end)
    for {
      span <- 2 to words.length
      begin:Int <- 0 to (words.length - span)
    } {
      val end = begin + span

      for ( a <- 0 until anchoring.topology.labelIndex.size if anchoring.sparsityPattern.bot.isAllowedLabeledSpan(begin, end, a)) {
        val numValidLabelRefs = anchoring.numValidRefinements(a)
        java.util.Arrays.fill(offsets, 0)

          val rules = anchoring.topology.indexedBinaryRulesWithParent(a)
          var ruleIndex = 0
          // into rules
          while (ruleIndex < rules.length) {
            val r = rules(ruleIndex)
            val b = anchoring.topology.leftChild(r)
            val c = anchoring.topology.rightChild(r)
            ruleIndex += 1

            val feasibleCoarseRange = inside.top.feasibleSplitPoints(begin, end, b, c)

            if (feasibleCoarseRange.nonEmpty) {
              val validA = anchoring.validParentRefinementsGivenRule(begin, feasibleCoarseRange.begin, feasibleCoarseRange.end, end, r)
              var ai = 0
              while (ai < validA.length) {
                val refA = validA(ai)
                ai += 1
                val spanScore = anchoring.scoreSpan(begin, end, a, refA)
                if (!spanScore.isInfinite) {

                  val refinements = anchoring.validRuleRefinementsGivenParent(begin, end, r, refA)
                  var ruleRefIndex = 0
                  while (ruleRefIndex < refinements.length) {
                    val refR = refinements(ruleRefIndex)
                    ruleRefIndex += 1
                    val refB = anchoring.leftChildRefinement(r, refR)
                    val refC = anchoring.rightChildRefinement(r, refR)

                    val feasibleSplitRange = inside.top.feasibleSplitPoints(begin, end, b, refB, c, refC)

                    var split = feasibleSplitRange.begin
                    val endSplit = feasibleSplitRange.end

                    while (split < endSplit) {
                      val bScore = inside.top.labelScore(begin, split, b, refB)
                      val cScore = inside.top.labelScore(split, end, c, refC)
                      val withoutRule = bScore + cScore + spanScore
                      if (withoutRule != Double.NegativeInfinity) {

                        val prob = withoutRule + anchoring.scoreBinaryRule(begin, split, end, r, refR)
                        assert(!prob.isNaN, s"$withoutRule ${anchoring.scoreBinaryRule(begin, split, end, r, refR)} $bScore $cScore $spanScore")

                        if (prob != Double.NegativeInfinity) {

                          scoreArray(refA)(offsets(refA)) = prob
                          offsets(refA) += 1
                          // buffer full
                          if (offsets(refA) == scoreArray(refA).length) {
                            scoreArray(refA)(0) = sum(scoreArray(refA), offsets(refA))
                            offsets(refA) = 1
                          }
                        }
                      }
                      split += 1
                    }
                  }
                }

              } // end a refinement

            } // end canBuildThisRule
          } // end rules

        enterScoresForLabelRefinements(sum, scoreArray, offsets, inside.bot, begin, end, a, numValidLabelRefs)
//          assert(rootScore(anchoring, inside, sum) != 0.0, (begin, end, a))
//          if (!foundSomething && refined.sparsityPattern != ChartConstraints.noSparsity) {
//            logger.warn(s"Failed to replicate a span in ($begin, $end) of ${anchoring.words}. Label is ${anchoring.grammar.labelIndex.get(a)}")
//
//          }

      }
      updateInsideUnaries(inside, anchoring, begin, end, sum)
    }
    inside
  }

  private def enterScoresForLabelRefinements[L](sum: Summer, scoreArray: Array[Array[Double]], offsets: Array[Int], bot: RefinedParseChart[L]#ChartScores, begin: Int, end: Int, parent: Int, numValidLabelRefs: Int) {
    var foundSomething = false
    var ai = 0
    while (ai < numValidLabelRefs) {
      // done updating vector, do an enter:
      if (offsets(ai) > 0) {
        val score = sum(scoreArray(ai), offsets(ai))
        assert(!score.isNaN, scoreArray(ai).take(offsets(ai)).toIndexedSeq)
        bot.enter(begin, end, parent, ai, score)
        foundSomething = true
      }
      ai += 1
    }
  }

  private def buildOutsideChart[L, W](anchoring: GrammarAnchoring[L, W],
                                      inside: RefinedParseChart[L], sum: Summer):RefinedParseChart[L] = {
    val refined = anchoring

    val grammar = anchoring.topology
    val rootIndex = grammar.labelIndex(grammar.root)

    val length = inside.length
    val outside = RefinedParseChart(grammar.labelIndex,
      Array.tabulate(grammar.labelIndex.size)(refined.numValidRefinements),
      length,
      refined.sparsityPattern)
    for(refRoot <- refined.validLabelRefinements(0, inside.length, rootIndex)) {
      outside.top.enter(0, inside.length, rootIndex, refRoot, 0.0)
    }
    updateOutsideUnaries(outside, inside, anchoring, 0, inside.length, sum)

    val scoreArray = Array.ofDim[Double](anchoring.maxLabelRefinements,  80)
    val offsets = new Array[Int](anchoring.maxLabelRefinements)

    for {
      span <- (length-1) until 0 by (-1)
      begin <- 0 to (length-span)
    } {
      val end = begin + span

      val enteredTop = inside.top.enteredLabelIndexes(begin, end)
      var a = 0
      while (a < grammar.labelIndex.size) {
        // we're going to populate a by looking at rules p -> a rc, p -> lc a
        if (enteredTop.contains(a)) {
          java.util.Arrays.fill(offsets, 0)
          doOutsideLeftCompletionUpdates(inside, outside, anchoring, begin, end, a, scoreArray, offsets, sum)
          doOutsideRightCompletionUpdates(inside, outside, anchoring, begin, end, a, scoreArray, offsets, sum)

          val numValidLabelRefs = anchoring.numValidRefinements(a)
          enterScoresForLabelRefinements(sum, scoreArray, offsets, outside.top, begin, end, a, numValidLabelRefs)
        }

        a += 1
      }
      updateOutsideUnaries(outside, inside, anchoring, begin, end, sum)
    }
    outside
  }

  private def doOutsideLeftCompletionUpdates[W, L](inside: RefinedParseChart[L], outside: RefinedParseChart[L],
                                                   anchoring: GrammarAnchoring[L, W],
                                                   begin: Int, end: Int,
                                                   label: Int,
                                                   scoreArray: Array[Array[Double]], offsets: Array[Int], sum: Summer) {
    val refined = anchoring
    val grammar = refined.topology
    val rules = anchoring.topology.indexedBinaryRulesWithLeftChild(label)

    var br = 0
    while (br < rules.length) {
      val r = rules(br)
      val p = grammar.parent(r)
      val rc = grammar.rightChild(r)
      br += 1

      // can I possibly build any refinement of this rule?
      val coarseCompletionSpan = feasibleSpanForCoarseLeftCompletion(begin, end, p, rc, inside)
      val coarseCompletionBegin = coarseCompletionSpan.begin
      val coarseCompletionEnd = coarseCompletionSpan.end

      val canBuildThisRule = coarseCompletionBegin <= coarseCompletionEnd

      if (canBuildThisRule) {

        val enteredRefTop = inside.top.enteredLabelRefinements(begin, end, label)
        val compatibleRefinements = refined.validLeftChildRefinementsGivenRule(begin, end, coarseCompletionBegin, coarseCompletionEnd, r)

        var refai = 0
        while (refai < compatibleRefinements.length) {
          val refA = compatibleRefinements(refai)
          if (enteredRefTop.contains(refA)) {
            val ruleRefinements = refined.validRuleRefinementsGivenLeftChild(begin, end, coarseCompletionBegin, coarseCompletionEnd, r, refA)
            var rfI = 0
            while (rfI < ruleRefinements.length) {
              val refR = ruleRefinements(rfI)
              rfI += 1
              val refP = refined.parentRefinement(r, refR)
              val refC = refined.rightChildRefinement(r, refR)

              val splitSpan = feasibleSpanForLeftCompletion(begin, end, p, refP, rc, refC, inside)

              var completion = splitSpan.begin

              while (completion <= splitSpan.end) {
                val pOutside = outside.bot.labelScore(begin, completion, p, refP) + anchoring.scoreSpan(begin, completion, p, refP)
                val cInside = inside.top.labelScore(end, completion, rc, refC)
                if (cInside != Double.NegativeInfinity && pOutside != Double.NegativeInfinity) {
                  val ruleScore = refined.scoreBinaryRule(begin, end, completion, r, refR)
                  val score = cInside + ruleScore + pOutside
                  if (score != Double.NegativeInfinity) {
                    scoreArray(refA)(offsets(refA)) = score
                    offsets(refA) += 1
                    // buffer full
                    if (offsets(refA) == scoreArray(refA).length) {
                      scoreArray(refA)(0) = sum(scoreArray(refA), offsets(refA))
                      offsets(refA) = 1
                    }
                  }
                }

                completion += 1
              }
            }
          }
          refai += 1
        }
      }
    }
  }


  private def feasibleSpanForLeftCompletion[L, W](begin: Int, end: Int, p: Int, refP: Int, rc: Int, refC: Int, inside: RefinedParseChart[L]) = {
    val parentMinCompletion = inside.bot.leftMostEndForBegin(begin)(p)(refP)
    val rcMinCompletion = inside.top.leftMostEndForBegin(end)(rc)(refC)
    val parentMaxCompletion = inside.bot.rightMostEndForBegin(begin)(p)(refP)
    val rcMaxCompletion = inside.top.rightMostEndForBegin(end)(rc)(refC)
    val completionBegin = math.max(math.max(parentMinCompletion, rcMinCompletion), end + 1)
    val completionEnd = math.min(parentMaxCompletion, rcMaxCompletion)
    Span(completionBegin, completionEnd)
  }

  private def feasibleSpanForCoarseLeftCompletion[L, W](begin: Int, end: Int, p: Int, rc: Int, inside: RefinedParseChart[L]) = {
    val parentMinCompletion = inside.bot.coarseLeftMostEndForBegin(begin)(p)
    val rcMinCompletion = inside.top.coarseLeftMostEndForBegin(end)(rc)
    val parentMaxCompletion = inside.bot.coarseRightMostEndForBegin(begin)(p)
    val rcMaxCompletion = inside.top.coarseRightMostEndForBegin(end)(rc)
    val completionBegin = math.max(math.max(parentMinCompletion, rcMinCompletion), end + 1)
    val completionEnd = math.min(parentMaxCompletion, rcMaxCompletion)
    Span(completionBegin, completionEnd)
  }

  private def doOutsideRightCompletionUpdates[W, L](inside: RefinedParseChart[L], outside: RefinedParseChart[L],
                                                    anchoring: GrammarAnchoring[L, W],
                                                    begin: Int, end: Int,
                                                    label: Int,
                                                    scoreArray: Array[Array[Double]],
                                                    offsets: Array[Int], sum: Summer) {
    val refined = anchoring
    val grammar = refined.topology
    val rules = anchoring.topology.indexedBinaryRulesWithRightChild(label)

    var br = 0
    while (br < rules.length) {
      val r = rules(br)
      val p = grammar.parent(r)
      val lc = grammar.leftChild(r)
      br += 1

      // can I possibly build any refinement of this rule?
      val coarseCompletionSpan = feasibleSpanForCoarseRightCompletion(begin, end, p, lc, inside)
      val coarseCompletionBegin = coarseCompletionSpan.begin
      val coarseCompletionEnd = coarseCompletionSpan.end
      val canBuildThisRule = coarseCompletionBegin <= coarseCompletionEnd

      if (canBuildThisRule) {
        val enteredRefTop = inside.top.enteredLabelRefinements(begin, end, label)
        val compatibleRefinements = refined.validRightChildRefinementsGivenRule(coarseCompletionBegin, coarseCompletionEnd, begin, end, r)

        var refai = 0
        while (refai < compatibleRefinements.length) {
          val refA = compatibleRefinements(refai)
          if (enteredRefTop.contains(refA)) {
            val ruleRefinements = refined.validRuleRefinementsGivenRightChild(coarseCompletionBegin, coarseCompletionEnd, begin, end, r, refA)
            var rfI = 0
            while (rfI < ruleRefinements.length) {
              val refR = ruleRefinements(rfI)
              rfI += 1
              val refP = refined.parentRefinement(r, refR)
              val refB = refined.leftChildRefinement(r, refR)

              val completionSpan = feasibleSpanForRightCompletion(begin, end, p, refP, lc, refB, inside)

              var completion = completionSpan.begin

              while (completion <= completionSpan.end) {
                val pOutside = outside.bot.labelScore(completion, end, p, refP) + anchoring.scoreSpan(completion, end, p, refP)
                val bInside = inside.top.labelScore(completion, begin, lc, refB)
                if (bInside != Double.NegativeInfinity && pOutside != Double.NegativeInfinity) {
                  val ruleScore = refined.scoreBinaryRule(completion, begin, end, r, refR)
                  val score = bInside + ruleScore + pOutside
                  if (score != Double.NegativeInfinity) {
                    scoreArray(refA)(offsets(refA)) = score
                    offsets(refA) += 1
                    // buffer full
                    if (offsets(refA) == scoreArray(refA).length) {
                      scoreArray(refA)(0) = sum(scoreArray(refA), offsets(refA))
                      offsets(refA) = 1
                    }
                  }
                }

                completion += 1
              }
            }
          }
          refai += 1
        }
      }
    }
  }

  private def feasibleSpanForRightCompletion[L, W](begin: Int, end: Int, p: Int, refP: Int, lc: Int, refB: Int, inside: RefinedParseChart[L]) = {
    val parentMinCompletion = inside.bot.leftMostBeginForEnd(end)(p)(refP)
    val rcMinCompletion = inside.top.leftMostBeginForEnd(begin)(lc)(refB)
    val parentMaxCompletion = inside.bot.rightMostBeginForEnd(end)(p)(refP)
    val rcMaxCompletion = inside.top.rightMostBeginForEnd(begin)(lc)(refB)
    val completionBegin = math.max(parentMinCompletion, rcMinCompletion)
    val completionEnd = math.min(begin, math.min(parentMaxCompletion, rcMaxCompletion))

    Span(completionBegin, completionEnd)
  }

  private def feasibleSpanForCoarseRightCompletion[L, W](begin: Int, end: Int, p: Int, lc: Int, inside: RefinedParseChart[L]) = {

    val parentMinCompletion = inside.bot.coarseLeftMostBeginForEnd(end)(p)
    val rcMinCompletion = inside.top.coarseLeftMostBeginForEnd(begin)(lc)
    val parentMaxCompletion = inside.bot.coarseRightMostBeginForEnd(end)(p)
    val rcMaxCompletion = inside.top.coarseRightMostBeginForEnd(begin)(lc)
    val completionBegin = math.max(parentMinCompletion, rcMinCompletion)
    val completionEnd = math.min(begin, math.min(parentMaxCompletion, rcMaxCompletion))

    Span(completionBegin, completionEnd)
  }

  private def updateInsideUnaries[L, W](chart: RefinedParseChart[L],
                                        anchoring: GrammarAnchoring[L, W],
                                        begin: Int, end: Int, sum: Summer) = {
    val refined = anchoring
    val grammar = anchoring.topology
    for(bi <- chart.bot.enteredLabelIndexes(begin, end); refB <- chart.bot.enteredLabelRefinements(begin, end, bi)) {
      val b = bi
      val bScore = chart.bot.labelScore(begin, end, b, refB)
      if (bScore != Double.NegativeInfinity) {
        val rules = grammar.indexedUnaryRulesWithChild(b)
        var j = 0
        while (j < rules.length) {
          val r = rules(j)
          val a = grammar.parent(r)
          if (refined.sparsityPattern.top.isAllowedLabeledSpan(begin, end, a)) {
            for (refR <- refined.validUnaryRuleRefinementsGivenChild(begin, end, r, refB)) {
              val refA = refined.parentRefinement(r, refR)
              val ruleScore: Double = refined.scoreUnaryRule(begin, end, r, refR)
              val prob: Double = bScore + ruleScore
              assert(!prob.isNaN, s"$ruleScore $bScore")
              if (prob != Double.NegativeInfinity) {
                chart.top.enter(begin, end, a, refA, sum(chart.top.labelScore(begin, end, a, refA), prob))
              }
            }
          }
          j += 1
        }
      }
    }

  }

  private def updateOutsideUnaries[L, W](chart: RefinedParseChart[L],
                                         inside: RefinedParseChart[L],
                                         anchoring: GrammarAnchoring[L, W],
                                         begin: Int, end: Int, sum: Summer) = {
    val refined = anchoring
    val grammar = anchoring.topology
    for(ai <- inside.top.enteredLabelIndexes(begin, end); refA <- inside.top.enteredLabelRefinements(begin, end, ai)) {
      val a = ai
      val bScore = chart.top.labelScore(begin, end, a, refA)
      val rules = grammar.indexedUnaryRulesWithParent(a)
      var j = 0
      while (j < rules.length) {
        val r = rules(j)
        val b = grammar.child(r)
        if (inside.bot.isLabelEntered(begin, end, b))
          for(refR <- refined.validRuleRefinementsGivenParent(begin, end, rules(j), refA)) {
            val refB = refined.childRefinement(rules(j), refR)
            val ruleScore: Double = refined.scoreUnaryRule(begin, end, rules(j), refR)
            val prob: Double = bScore + ruleScore
            if (prob != Double.NegativeInfinity) {
              chart.bot.enter(begin, end, b, refB, sum(chart.bot.labelScore(begin, end, b, refB), prob))
            }
          }
        j += 1
      }
    }

  }

}


