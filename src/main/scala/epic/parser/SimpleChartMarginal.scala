package epic.parser

import breeze.util.Index
import breeze.linalg.{softmax, max, DenseMatrix}
import breeze.collection.mutable.TriangularArray

/**
 * TODO
 *
 * @author dlwh
final case class SimpleChartMarginal[L, L2, W](anchoring: SimpleGrammar.Anchoring[L, L2, W],
                                               inside: SimpleParseChart[L2], outside: SimpleParseChart[L2],
                                               isMaxMarginal: Boolean = true) extends ParseMarginal[L, W] {
  override val logPartition: Double = {
    val scores = anchoring.refinements.labels.refinementsOf(anchoring.topology.rootIndex).map(inside.top.labelScore(0, length, _))
    if(isMaxMarginal) max(scores)
    else softmax(scores)
  }

  override def insideTopScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    inside.top.labelScore(begin, end, anchoring.refinements.labels.globalize(sym, ref))
  }

  override def insideBotScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    inside.top.labelScore(begin, end, anchoring.refinements.labels.globalize(sym, ref))
  }

  override def feasibleSplitPoints(begin: Int, end: Int, leftChild: Int, leftChildRef: Int, rightChild: Int, rightChildRef: Int): IndexedSeq[Int] = {
    (begin + 1) until (end)
  }

  override def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double): Unit = {
    if(logPartition.isInfinite) throw new RuntimeException("No parse for " + words)
    if(logPartition.isNaN) throw new RuntimeException("NaN prob!")

    val lexLoc = anchoring.lexicon.anchor(anchoring.words)
    // handle lexical
    for (i <- 0 until words.length) {
      var visitedSomething  = false
      for {
        a <- lexLoc.allowedTags(i)
        ref <- anchoring.validLabelRefinements(i, i+ 1, a)
      } {
        val aa = anchoring.refinements.labels.globalize(a, ref)
        val score:Double = anchoring.scoreSpan(i, i+1, a, ref) + outside.bot(i, i+1, aa) - logPartition
        assert(!score.isNaN, s"${anchoring.scoreSpan(i, i + 1, a, ref)} ${outside.bot(i, i + 1, aa)} $logPartition")
        if (score != Double.NegativeInfinity) {
          spanVisitor.visitSpan(i, i+1, a, ref, math.exp(score))
          visitedSomething = true
        }
      }


    }

    // handle binaries
    for {
      span <- 2 to length
      begin <- 0 to (length - span)
      parent <- 0 until anchoring.topology.labelIndex.size
    } {
      val end = begin + span
      val aOutside = outside.bot(begin, end, parent)
      val labelMarginal = inside.bot(begin, end, parent) + aOutside - logPartition
      if(labelMarginal > spanThreshold) {
        val aCoarse = anchoring.refinements.labels.project(parent)
        val aRef = anchoring.refinements.labels.localize(parent)
        spanVisitor.visitSpan(begin, end, aCoarse, aRef, math.exp(labelMarginal))
        if(!spanVisitor.skipBinaryRules) {
          val rules = anchoringTopology.indexedBinaryRulesWithParent(parent)
          var i = 0
          while(i < rules.length) {
            val r = rules(i)
            val b = topology.leftChild(r)
            val c = topology.rightChild(r)

            var split = begin + 1
            while(split < end) {
              val bInside = inside.top.labelScore(begin, split, b)
              val cInside = inside.top.labelScore(split, end, c)
              val ruleScore = anchoring.grammar.ruleScore(r)

              val coarseR = anchoring.refinements.rules.project(r)
              val refR = anchoring.refinements.rules.localize(r)

              val margScore = bInside + cInside + ruleScore + aOutside - logPartition

              if(margScore != Double.NegativeInfinity) {
                spanVisitor.visitBinaryRule(begin, split, end, coarseR, refR, math.exp(margScore))
              }

              split += 1
            }

            i += 1
          }

        }

      }
    }

    if(!spanVisitor.skipUnaryRules)
      for {
        span <- 1 to words.length
        begin <- 0 to (words.length - span)
        end = begin + span
        parent <- 0 until anchoring.topology.labelIndex.size
      } {
        val end = begin + span
        val aOutside = outside.top(begin, end, parent)
        val labelMarginal = inside.top(begin, end, parent) + aOutside - logPartition
        if (labelMarginal > spanThreshold) {

          for (r <- anchoringTopology.indexedUnaryRulesWithParent(parent)) {
            val b = topology.child(r)
            val bScore = inside.bot.labelScore(begin, end, b)
            val rScore = anchoring.grammar.ruleScore(r)
            val prob = math.exp(bScore + aOutside + rScore - logPartition)
            val refR = anchoring.refinements.rules.localize(r)
            val projR = anchoring.refinements.rules.project(r)
            if (prob > 0)
              spanVisitor.visitUnaryRule(begin, end, projR, refR, prob)
          }
        }
      }

  }
}

object SimpleChartMarginal {
  import RefinedChartMarginal._

  def apply[L, L2, W](grammar: SimpleGrammar[L, L2, W], words: IndexedSeq[W], maxMarginal: Boolean = false): SimpleChartMarginal[L, L2, W] = {
    val anchoring = grammar.anchor(words)
    val sum = if(maxMarginal) MaxSummer else LogSummer
    val inside = buildInsideChart(anchoring, sum)
    val outside = buildOutsideChart(anchoring, inside, sum)
    SimpleChartMarginal(anchoring, inside, outside, maxMarginal)
  }



  // first parse chart is the inside scores, second parse chart is span scores for spans that were computed.
  private def buildInsideChart[L, L2, W](anchoring: SimpleGrammar.Anchoring[L, L2, W], sum: Summer): SimpleParseChart[L] = {
    import anchoring._

    val grammar = anchoring.topology

    val inside = new SimpleParseChart(topology.labelIndex, words.length)
    val tagConstraints = anchoring.tagConstraints

    // handle lexical
    for{i <- 0 until words.length} {
      var foundSomething = false
      for {
        a <- tagConstraints.allowedTags(i) if sparsityPattern.bot.isAllowedLabeledSpan(i, i+1, a)
        ref <- anchoring.validLabelRefinements(i, i+1, a)
      } {
        val score:Double = anchoring.scoreSpan(i, i+1, a, ref)
        val glob = refinements.labels.globalize(a, ref)
        if (score != Double.NegativeInfinity) {
          inside.bot.enter(i, i+1, glob, score)
          foundSomething = true
        }
      }

      updateInsideUnaries(inside, anchoring,  i, i+1, sum)
    }

    val g = grammar

    val scoreArray = Array.ofDim[Double](maxLabelRefinements,  40)
    val offsets = new Array[Int](anchoring.maxLabelRefinements)

    // a -> bc over [begin, split, end)
    for {
      span <- 2 to words.length
      begin:Int <- 0 to (words.length - span)
    } {
      val end = begin + span

      for ( a <- 0 until grammar.labelIndex.size if sparsityPattern.top.isAllowedLabeledSpan(begin, end, a)) {
        val numValidLabelRefs = anchoring.numValidRefinements(a)
        java.util.Arrays.fill(offsets, 0)
          val rules = anchoring.topology.indexedBinaryRulesWithParent(a)
          var ruleIndex = 0
          // into rules
          while(ruleIndex < rules.length) {
            val r = rules(ruleIndex)
            val b = g.leftChild(r)
            val c = g.rightChild(r)
            ruleIndex += 1


            val canBuildThisRule = true //narrowR < end && narrowL >= narrowR && coarseSplitBegin <= narrowL && coarseSplitBegin < coarseSplitEnd

            if(canBuildThisRule) {

              var refA = 0
              while(refA < numValidLabelRefs) {

                val refinements = anchoring.validRuleRefinementsGivenParent(begin, end, r, refA)
                var ruleRefIndex = 0
                while(ruleRefIndex < refinements.length) {
                  val refR = refinements(ruleRefIndex)
                  ruleRefIndex += 1
                  val refB = anchoring.leftChildRefinement(r, refR)
                  val refC = anchoring.rightChildRefinement(r, refR)

                  var split = begin + 1
                  val endSplit = end

                  while(split < endSplit) {
                    val bScore = inside.top.labelScore(begin, split, b, refB)
                    val cScore = inside.top.labelScore(split, end, c, refC)
                    val withoutRule = bScore + cScore + spanScore
                    if(withoutRule != Double.NegativeInfinity) {

                      val prob = withoutRule + anchoring.scoreBinaryRule(begin, split, end, r, refR)

                      scoreArray(refA)(offsets(refA)) = prob
                      offsets(refA) += 1
                      // buffer full
                      if(offsets(refA) == scoreArray(refA).length) {
                        scoreArray(refA)(0) = sum(scoreArray(refA), offsets(refA))
                        offsets(refA) = 1
                      }
                    }
                    split += 1
                  }
                }

                refA += 1
              } // end a refinement

            } // end canBuildThisRule
          } // end rules

          var foundSomething = false
          var ai = 0
          while(ai < numValidLabelRefs) {
            // done updating vector, do an enter:
            if(offsets(ai) > 0) {
              val score = sum(scoreArray(ai), offsets(ai))
              inside.bot.enter(begin, end, a, ai, score)
              foundSomething = true
            }
            ai += 1
          }
          //          assert(rootScore(anchoring, inside, sum) != 0.0, (begin, end, a))
          //          if(!foundSomething && anchoring.core.sparsityPattern != ChartConstraints.noSparsity) {
          //            logger.warn(s"Failed to replicate a span in ($begin, $end) of ${anchoring.words}. Label is ${anchoring.grammar.labelIndex.get(a)}")
          //
          //          }

      }
      updateInsideUnaries(inside, anchoring, begin, end, sum)
    }
    inside
  }

  private def buildOutsideChart[L, W](anchoring: AugmentedAnchoring[L, W],
                                      inside: RefinedParseChart[L], spanScores: RefinedParseChart[L], sum: Summer):RefinedParseChart[L] = {
    val refined = anchoring
    val core = anchoring.core

    val grammar = anchoring.topology
    val rootIndex = grammar.labelIndex(grammar.root)

    // cache for coreScores
    val coreScoreArray = new Array[Double](inside.length + 1)

    val length = inside.length
    val outside = RefinedParseChart.logProb.apply(grammar.labelIndex,
      Array.tabulate(grammar.labelIndex.size)(refined.numValidRefinements),
      length,
      core.sparsityPattern)
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
      while(a < grammar.labelIndex.size) {
        // we're going to populate a by looking at rules p -> a rc, p -> lc a
        if (enteredTop.contains(a)) {
          java.util.Arrays.fill(offsets, 0)
          doOutsideLeftCompletionUpdates(inside, outside, spanScores, anchoring, begin, end, a, coreScoreArray, scoreArray, offsets, sum)
          doOutsideRightCompletionUpdates(inside, outside, spanScores, anchoring, begin, end, a, coreScoreArray, scoreArray, offsets, sum)

          val numValidLabelRefs = anchoring.numValidRefinements(a)
          var ai = 0
          while(ai < numValidLabelRefs) {
            // done updating vector, do an enter:
            if(offsets(ai) > 0) {
              outside.top.enter(begin, end, a, ai, sum(scoreArray(ai), offsets(ai)))
            }
            ai += 1
          }
        }

        a += 1
      }
      updateOutsideUnaries(outside, inside, anchoring, begin, end, sum)
    }
    outside
  }


  private def doOutsideLeftCompletionUpdates[W, L](inside: RefinedParseChart[L], outside: RefinedParseChart[L],
                                                   spanScores: RefinedParseChart[L],
                                                   anchoring: AugmentedAnchoring[L, W],
                                                   begin: Int, end: Int,
                                                   label: Int,
                                                   coreScoreArray: Array[Double],
                                                   scoreArray: Array[Array[Double]], offsets: Array[Int], sum: Summer) {
    val refined = anchoring
    val itop = inside.top
    val grammar = refined.topology
    val rules = anchoring.topology.indexedBinaryRulesWithLeftChild(label)
    val length = inside.length


    var br = 0
    while (br < rules.length) {
      val r = rules(br)
      val p = grammar.parent(r)
      val rc = grammar.rightChild(r)
      br += 1

      // can I possibly build any refinement of this rule?
      val parentMinCompletion = inside.bot.coarseLeftMostEndForBegin(begin)(p)
      val rcMinCompletion = inside.top.coarseLeftMostEndForBegin(end)(rc)
      val parentMaxCompletion = inside.bot.coarseRightMostEndForBegin(begin)(p)
      val rcMaxCompletion = inside.top.coarseRightMostEndForBegin(end)(rc)
      val coarseCompletionBegin = math.max(math.max(parentMinCompletion, rcMinCompletion), end + 1)
      val coarseCompletionEnd = math.min(parentMaxCompletion, rcMaxCompletion)
      val canBuildThisRule = coarseCompletionBegin <= coarseCompletionEnd
      assert(coarseCompletionBegin > end)
      assert(coarseCompletionEnd <= length, coarseCompletionEnd + " " + length)


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

              val parentMinCompletion = inside.bot.leftMostEndForBegin(begin)(p)(refP)
              val rcMinCompletion = inside.top.leftMostEndForBegin(end)(rc)(refC)
              val parentMaxCompletion = inside.bot.rightMostEndForBegin(begin)(p)(refP)
              val rcMaxCompletion = inside.top.rightMostEndForBegin(end)(rc)(refC)
              val completionBegin = math.max(math.max(parentMinCompletion, rcMinCompletion), end + 1)
              val completionEnd = math.min(parentMaxCompletion, rcMaxCompletion)

              var completion = completionBegin

              while (completion <= completionEnd) {
                val pOutside = outside.bot.labelScore(begin, completion, p, refP) + spanScores.bot.labelScore(begin, completion, p, refP)
                val cInside = itop.labelScore(end, completion, rc, refC)
                if (cInside != Double.NegativeInfinity && pOutside != Double.NegativeInfinity) {
                  val ruleScore = refined.scoreBinaryRule(begin, end, completion, r, refR) + coreScoreArray(completion)
                  val score = cInside + ruleScore + pOutside
                  scoreArray(refA)(offsets(refA)) = score
                  offsets(refA) += 1
                  // buffer full
                  if (offsets(refA) == scoreArray(refA).length) {
                    scoreArray(refA)(0) = sum(scoreArray(refA), offsets(refA))
                    offsets(refA) = 1
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

  private def doOutsideRightCompletionUpdates[W, L](inside: RefinedParseChart[L], outside: RefinedParseChart[L],
                                                    spanScores: RefinedParseChart[L],
                                                    anchoring: AugmentedAnchoring[L, W],
                                                    begin: Int, end: Int,
                                                    label: Int,
                                                    coreScoreArray: Array[Double],
                                                    scoreArray: Array[Array[Double]],
                                                    offsets: Array[Int], sum: Summer) {
    val refined = anchoring
    val itop = inside.top
    val grammar = refined.topology
    val rules = anchoring.topology.indexedBinaryRulesWithRightChild(label)

    var br = 0
    while (br < rules.length) {
      val r = rules(br)
      val p = grammar.parent(r)
      val lc = grammar.leftChild(r)
      br += 1

      // can I possibly build any refinement of this rule?
      val parentMinCompletion = inside.bot.coarseLeftMostBeginForEnd(end)(p)
      val rcMinCompletion = inside.top.coarseLeftMostBeginForEnd(begin)(lc)
      val parentMaxCompletion = inside.bot.coarseRightMostBeginForEnd(end)(p)
      val rcMaxCompletion = inside.top.coarseRightMostBeginForEnd(begin)(lc)
      val coarseCompletionBegin = math.max(parentMinCompletion, rcMinCompletion)
      val coarseCompletionEnd = math.min(begin, math.min(parentMaxCompletion, rcMaxCompletion))
      val canBuildThisRule = coarseCompletionBegin <= coarseCompletionEnd

      if (canBuildThisRule) {
        // initialize core scores
        RefinedChartMarginal.fillCoreScoresForRightCompletion(coreScoreArray,
          begin, end,
          anchoring.core,
          coarseCompletionBegin, coarseCompletionEnd,
          r)

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

              val parentMinCompletion = inside.bot.leftMostBeginForEnd(end)(p)(refP)
              val rcMinCompletion = inside.top.leftMostBeginForEnd(begin)(lc)(refB)
              val parentMaxCompletion = inside.bot.rightMostBeginForEnd(end)(p)(refP)
              val rcMaxCompletion = inside.top.rightMostBeginForEnd(begin)(lc)(refB)
              val completionBegin = math.max(parentMinCompletion, rcMinCompletion)
              val completionEnd = math.min(begin,math.min(parentMaxCompletion, rcMaxCompletion))

              var completion = completionBegin

              while (completion <= completionEnd) {
                val pOutside = outside.bot.labelScore(completion, end, p, refP) + spanScores.bot.labelScore(completion, end, p, refP)
                val bInside = itop.labelScore(completion, begin, lc, refB)
                if (bInside != Double.NegativeInfinity && pOutside != Double.NegativeInfinity) {
                  val ruleScore = refined.scoreBinaryRule(completion, begin, end, r, refR) + coreScoreArray(completion)
                  val score = bInside + ruleScore + pOutside
                  scoreArray(refA)(offsets(refA)) = score
                  offsets(refA) += 1
                  // buffer full
                  if (offsets(refA) == scoreArray(refA).length) {
                    scoreArray(refA)(0) = sum(scoreArray(refA), offsets(refA))
                    offsets(refA) = 1
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

  private def updateInsideUnaries[L, W](chart: RefinedParseChart[L],
                                        anchoring: AugmentedAnchoring[L, W],
                                        begin: Int, end: Int, sum: Summer) = {
    val refined = anchoring
    val core = anchoring.core
    val grammar = anchoring.topology
    for(bi <- chart.bot.enteredLabelIndexes(begin, end); refB <- chart.bot.enteredLabelRefinements(begin, end, bi)) {
      val b = bi
      val bScore = chart.bot.labelScore(begin, end, b, refB)
      if(bScore != Double.NegativeInfinity) {
        val rules = grammar.indexedUnaryRulesWithChild(b)
        var j = 0
        while(j < rules.length) {
          val r = rules(j)
          val a = grammar.parent(r)
          if(core.sparsityPattern.top.isAllowedLabeledSpan(begin, end, a)) {
            val coreScore = core.scoreUnaryRule(begin, end, r)
            if(coreScore != Double.NegativeInfinity)
              for(refR <- refined.validUnaryRuleRefinementsGivenChild(begin, end, r, refB)) {
                val refA = refined.parentRefinement(r, refR)
                val ruleScore: Double = refined.scoreUnaryRule(begin, end, r, refR) + coreScore
                val prob: Double = bScore + ruleScore
                if(prob != Double.NegativeInfinity) {
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
                                         anchoring: AugmentedAnchoring[L, W],
                                         begin: Int, end: Int, sum: Summer) = {
    val refined = anchoring
    val core = anchoring.core
    val grammar = anchoring.topology
    for(ai <- inside.top.enteredLabelIndexes(begin, end); refA <- inside.top.enteredLabelRefinements(begin, end, ai)) {
      val a = ai
      val bScore = chart.top.labelScore(begin, end, a, refA)
      val rules = grammar.indexedUnaryRulesWithParent(a)
      var j = 0
      while(j < rules.length) {
        val r = rules(j)
        val coreScore = core.scoreUnaryRule(begin, end, r)
        if(coreScore != Double.NegativeInfinity) {
          val b = grammar.child(r)
          if(inside.bot.isLabelEntered(begin, end, b))
            for(refR <- refined.validRuleRefinementsGivenParent(begin, end, rules(j), refA)) {
              val refB = refined.childRefinement(rules(j), refR)
              val ruleScore: Double = refined.scoreUnaryRule(begin, end, rules(j), refR) + coreScore
              val prob: Double = bScore + ruleScore
              if(prob != Double.NegativeInfinity) {
                chart.bot.enter(begin, end, b, refB, sum(chart.bot.labelScore(begin, end, b, refB), prob))
              }
            }
        }
        j += 1
      }
    }

  }


}


@SerialVersionUID(1)
final class SimpleParseChart[L](val index: Index[L], val length: Int) extends Serializable {

  val top, bot = new ChartScores()

  final class ChartScores private[SimpleParseChart]() {
    val scores = DenseMatrix.zeros[Double](index.size, TriangularArray.arraySize(length + 1))
    scores := Double.NegativeInfinity
    @inline def labelScore(begin: Int, end: Int, label: Int) = scores(label, TriangularArray.index(begin, end))
    @inline def apply(begin: Int, end: Int, label: Int) = scores(label, TriangularArray.index(begin, end))

    def enter(begin: Int, end: Int, parent: Int, w: Double): Unit = {
      scores(parent, TriangularArray.index(begin, end)) = w
    }

  }

}



 **/
