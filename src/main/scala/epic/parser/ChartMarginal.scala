package epic.parser

import epic.trees.{UnaryTree, BinarizedTree}
import reflect.ClassTag
import epic.util.Arrays

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
case class ChartMarginal[L, W](anchoring: AugmentedAnchoring[L, W],
                               inside: ParseChart[L], outside: ParseChart[L],
                               logPartition: Double) extends ParseMarginal[L, W] {

  def checkForTree(tree: BinarizedTree[(L, Int)]) = {
    for (t <- tree.allChildren) t match {
      case UnaryTree( (label, ref), _, _, span) =>
        val labelScore = inside.top(span.start, span.end, anchoring.grammar.labelIndex(label), ref)
        if (labelScore.isInfinite) {
          println("problem with unary: " + (label, ref) + " " + span)
        }
      case _ =>
        val labelScore = inside.bot(tree.span.start, tree.span.end, anchoring.grammar.labelIndex(tree.label._1), tree.label._2)
        if (labelScore.isInfinite) {
          println("problem with other: " + t.label + " " + tree.span)
        }
    }
    this
  }


  /**
   * Forest traversal that visits spans in a "bottom up" order.
   */
  def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double = Double.NegativeInfinity) {
    if(logPartition.isInfinite) throw new RuntimeException("No parse for " + words)
    val itop = inside.top

    // handle lexical
    for (i <- 0 until words.length) {
      for {
        aa <- lexicon.tagsForWord(words(i))
        a = grammar.labelIndex(aa)
        ref <- anchoring.refined.validLabelRefinements(i, i+ 1, a)
      } {
        val score:Double = anchoring.scoreSpan(i, i+1, a, ref) + outside.bot(i, i+1, a, ref) - logPartition
        if (score != Double.NegativeInfinity) {
          spanVisitor.visitSpan(i, i+1, a, ref, math.exp(score))
        }
      }
    }

    // cache to hold core scores for binary rules
    val coreScoreArray = new Array[Double](words.length)

    // handle binaries
    for {
      span <- 2 to inside.length
      begin <- 0 to (inside.length - span)
    } {
      val end = begin + span

      // I get a 20% speedup if I inline these arrays. so be it.
      val narrowRight = inside.top.narrowRight(begin)
      val narrowLeft = inside.top.narrowLeft(end)
      val wideRight = inside.top.wideRight(begin)
      val wideLeft = inside.top.wideLeft(end)

      val coarseNarrowRight = inside.top.coarseNarrowRight(begin)
      val coarseNarrowLeft = inside.top.coarseNarrowLeft(end)
      val coarseWideRight = inside.top.coarseWideRight(begin)
      val coarseWideLeft = inside.top.coarseWideLeft(end)

      for (a <- inside.bot.enteredLabelIndexes(begin, end); refA <- inside.bot.enteredLabelRefinements(begin, end, a)) {
        var i = 0
        val aOutside = outside.bot.labelScore(begin, end, a, refA)
        val labelMarginal = aOutside + inside.bot.labelScore(begin, end, a, refA) - logPartition
        val aScore = aOutside + anchoring.scoreSpan(begin, end, a, refA)
        if(labelMarginal > spanThreshold) {
          spanVisitor.visitSpan(begin, end, a, refA, math.exp(labelMarginal))

          val rules = anchoring.refined.validCoarseRulesGivenParentRefinement(a, refA)
          while(i < rules.length) {
            val r = rules(i)
            val b = grammar.leftChild(r)
            val c = grammar.rightChild(r)
            i += 1

            val narrowR = coarseNarrowRight(b)
            val narrowL = coarseNarrowLeft(c)
            val coarseSplitBegin = math.max(narrowR, coarseWideLeft(c))
            val coarseSplitEnd = math.min(coarseWideRight(b), narrowL) + 1
            val canBuildThisRule = narrowR < end && narrowL >= narrowR && coarseSplitBegin <= narrowL && coarseSplitBegin < coarseSplitEnd

            if(canBuildThisRule) {
              // initialize core scores
              ChartMarginal.fillCoreScores(coreScoreArray,
                begin, end,
                anchoring.core,
                coarseSplitBegin, coarseSplitEnd,
                r,
                inside)

              val refinements = anchoring.refined.validRuleRefinementsGivenParent(begin, end, r, refA)
              var ruleRefIndex = 0
              while(ruleRefIndex < refinements.length) {
                val refR = refinements(ruleRefIndex)
                ruleRefIndex += 1
                val refB = anchoring.refined.leftChildRefinement(r, refR)
                val refC = anchoring.refined.rightChildRefinement(r, refR)

                val narrowR = narrowRight(b)(refB)
                val narrowL = narrowLeft(c)(refC)
                var split = math.max(narrowR, wideLeft(c)(refC))
                val endSplit = math.min(wideRight(b)(refB), narrowL) + 1
                val canBuildThisRule = narrowR < end && narrowL >= narrowR && split <= narrowL && split < endSplit
                if(!canBuildThisRule)
                  split = endSplit

                while(split < endSplit) {
                  val bInside = itop.labelScore(begin, split, b, refB)
                  val cInside = itop.labelScore(split, end, c, refC)
                  val coreScore = coreScoreArray(split)
                  val withoutRefined = bInside + cInside + coreScore
                  if (!java.lang.Double.isInfinite(withoutRefined)) {
                    val ruleScore = anchoring.refined.scoreBinaryRule(begin, split, end, r, refR)
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

    // Unaries
    for {
      span <- 1 to words.length
      begin <- 0 to (words.length - span)
      end = begin + span
      a <- inside.top.enteredLabelIndexes(begin, end)
      refA <- inside.top.enteredLabelRefinements(begin, end, a)
    } {
      val aScore = outside.top.labelScore(begin, end, a, refA)
      for (r <- grammar.indexedUnaryRulesWithParent(a); refR <- anchoring.refined.validRuleRefinementsGivenParent(begin, end, r, refA)) {
        val b = grammar.child(r)
        val refB = anchoring.refined.childRefinement(r, refR)
        val bScore = inside.bot.labelScore(begin, end, b, refB)
        val rScore = anchoring.scoreUnaryRule(begin, end, r, refR)
        val prob = math.exp(bScore + aScore + rScore - logPartition)
        if (prob > 0)
          spanVisitor.visitUnaryRule(begin, end, r, refR, prob)
      }
    }
  }

}

object ChartMarginal {
  def apply[L, W, Chart[X] <: ParseChart[X]](grammar: AugmentedGrammar[L, W],
                                             sent: Seq[W]): ChartMarginal[L, W] = {
    apply(grammar.anchor(sent), sent)
  }

  def apply[L, W, Chart[X] <: ParseChart[X]](anchoring: AugmentedAnchoring[L, W],
                                             sent: Seq[W]): ChartMarginal[L, W] = {
    val inside = buildInsideChart(anchoring, sent)
    val outside = buildOutsideChart(anchoring, inside)
    val logPartition = rootScore(anchoring, inside)
    ChartMarginal(anchoring, inside, outside, logPartition)
  }


  private def rootScore[L, W](anchoring: AugmentedAnchoring[L, W], inside: ParseChart[L]): Double = {
    val rootIndex: Int = anchoring.grammar.labelIndex(anchoring.grammar.root)
    val rootScores = new Array[Double](anchoring.refined.validLabelRefinements(0, inside.length, rootIndex).length)
    var offset = 0
    for(ref <- inside.top.enteredLabelRefinements(0, inside.length, rootIndex)) {
      val score = inside.top.labelScore(0, inside.length, rootIndex, ref)
      if(score != Double.NegativeInfinity) {
        rootScores(offset) = score
        offset += 1
      }
    }
    val score = inside.sum(rootScores, offset)
    assert(!score.isNaN)
    score
  }

  private def buildInsideChart[L, W, Chart[X] <: ParseChart[X]](anchoring: AugmentedAnchoring[L, W],
                                                                words: Seq[W]): ParseChart[L] = {
    val refined = anchoring.refined
    val core = anchoring.core

    val grammar = anchoring.grammar
    val lexicon = anchoring.lexicon

    val inside = ParseChart.logProb.apply(grammar.labelIndex,
      Array.tabulate(grammar.labelIndex.size)(refined.numValidRefinements),
      words.length,
      core.sparsityPattern)
    for{i <- 0 until words.length} {
      var foundSomething = false
      for {
        aa <- lexicon.tagsForWord(words(i))
        a = grammar.labelIndex(aa)
        coreScore = core.scoreSpan(i, i+1, a) if coreScore != Double.NegativeInfinity
        ref <- refined.validLabelRefinements(i, i+1, a)
      } {
        val score:Double = refined.scoreSpan(i, i+1, a, ref) + coreScore
        if (score != Double.NegativeInfinity) {
          inside.bot.enter(i, i+1, a, ref, score)
          foundSomething = true
        }
      }

      updateInsideUnaries(inside, anchoring,  i, i+1)
    }


    // cache for coreScores
    val coreScoreArray = new Array[Double](words.length)

    val top = inside.top
    val g = grammar

    // a -> bc over [begin, split, end)
    for {
      span <- 2 to words.length
      begin <- 0 to (words.length - span)
    } {
      val end = begin + span
      // I get a 20% speedup by inlining code dealing with these arrays. sigh.
      val narrowRight = top.narrowRight(begin)
      val narrowLeft = top.narrowLeft(end)
      val wideRight = top.wideRight(begin)
      val wideLeft = top.wideLeft(end)

      val coarseNarrowRight = top.coarseNarrowRight(begin)
      val coarseNarrowLeft = top.coarseNarrowLeft(end)
      val coarseWideRight = top.coarseWideRight(begin)
      val coarseWideLeft = top.coarseWideLeft(end)
      val scoreArray = Arrays.newArray(anchoring.refined.maxLabelRefinements,  20)
      val offsets = new Array[Int](anchoring.refined.maxLabelRefinements)

      for ( a <- 0 until grammar.labelIndex.size ) {
        val numValidLabelRefs = anchoring.refined.numValidRefinements(a)
        java.util.Arrays.fill(offsets, 0)

        val coreSpan = core.scoreSpan(begin, end, a)
        if (coreSpan != Double.NegativeInfinity) {
          val rules = anchoring.grammar.indexedBinaryRulesWithParent(a)
          var ruleIndex = 0
          // into rules
          while(ruleIndex < rules.length) {
            val r = rules(ruleIndex)
            val b = g.leftChild(r)
            val c = g.rightChild(r)
            ruleIndex += 1

            // Check: can we build any refinement of this rule?
            // basically, we can if TODO
            val narrowR = coarseNarrowRight(b)
            val narrowL = coarseNarrowLeft(c)
            val coarseSplitBegin = math.max(narrowR, coarseWideLeft(c))
            val coarseSplitEnd = math.min(coarseWideRight(b), narrowL) + 1
            val canBuildThisRule = narrowR < end && narrowL >= narrowR && coarseSplitBegin <= narrowL && coarseSplitBegin < coarseSplitEnd

            if(canBuildThisRule) {
              // initialize core scores
              ChartMarginal.fillCoreScores(coreScoreArray,
                begin, end,
                anchoring.core,
                coarseSplitBegin, coarseSplitEnd,
                r,
                inside)

              val validA = refined.validParentRefinementsGivenRule(begin, coarseSplitBegin, coarseSplitEnd, end, r)
              var ai = 0
              while(ai < validA.length) {
                val refA = validA(ai)
                ai += 1
                val passScore = refined.scoreSpan(begin, end, a, refA) + coreSpan
                if(!passScore.isInfinite) {
                  val refinements = refined.validRuleRefinementsGivenParent(begin, end, r, refA)
                  var ruleRefIndex = 0
                  while(ruleRefIndex < refinements.length) {
                    val refR = refinements(ruleRefIndex)
                    ruleRefIndex += 1
                    val refB = refined.leftChildRefinement(r, refR)
                    val refC = refined.rightChildRefinement(r, refR)

                    val narrowR = narrowRight(b)(refB)
                    val narrowL = narrowLeft(c)(refC)
                    var split = math.max(narrowR, wideLeft(c)(refC))
                    val endSplit = math.min(wideRight(b)(refB), narrowL) + 1
                    val canBuildThisRule = narrowR < end && narrowL >= narrowR && split <= narrowL && split < endSplit
                    if(!canBuildThisRule)
                      split = endSplit

                    while(split < endSplit) {
                      val bScore = inside.top.labelScore(begin, split, b, refB)
                      val cScore = inside.top.labelScore(split, end, c, refC)
                      val withoutRule = bScore + cScore + passScore + coreScoreArray(split)
                      if(withoutRule != Double.NegativeInfinity) {

                        val prob = withoutRule + refined.scoreBinaryRule(begin, split, end, r, refR)

                        scoreArray(refA)(offsets(refA)) = prob
                        offsets(refA) += 1
                        // buffer full
                        if(offsets(refA) == scoreArray(refA).length) {
                          scoreArray(refA)(0) = inside.sum(scoreArray(refA), offsets(refA))
                          offsets(refA) = 1
                        }
                      }
                      split += 1
                    }
                  }
                }

              } // end a refinement



            } // end canBuildThisRule
          } // end rules

          var ai = 0
          while(ai < numValidLabelRefs) {
            // done updating vector, do an enter:
            if(offsets(ai) > 0) {
              inside.bot.enterSum(begin, end, a, ai, scoreArray(ai), offsets(ai))
            }
            ai += 1
          }

        }
      }
      updateInsideUnaries(inside, anchoring, begin, end)
    }
    inside
  }

  private def buildOutsideChart[L, W](anchoring: AugmentedAnchoring[L, W],
                                      inside: ParseChart[L]):ParseChart[L] = {
    val refined = anchoring.refined
    val core = anchoring.core

    val grammar = anchoring.grammar
    val rootIndex = grammar.labelIndex(grammar.root)

    // cache for coreScores
    val coreScoreArray = new Array[Double](inside.length)

    val length = inside.length
    val outside = ParseChart.logProb.apply(grammar.labelIndex,
      Array.tabulate(grammar.labelIndex.size)(refined.numValidRefinements),
      length,
      core.sparsityPattern)
    for(refRoot <- refined.validLabelRefinements(0, inside.length, rootIndex)) {
      outside.top.enter(0, inside.length, rootIndex, refRoot, 0.0)
    }
    val itop = inside.top
    for {
      span <- length until 0 by (-1)
      begin <- 0 to (length-span)
    } {
      val end = begin + span
      val narrowRight = itop.narrowRight(begin)
      val narrowLeft = itop.narrowLeft(end)
      val wideRight = itop.wideRight(begin)
      val wideLeft = itop.wideLeft(end)

      val coarseNarrowRight = inside.top.coarseNarrowRight(begin)
      val coarseNarrowLeft = inside.top.coarseNarrowLeft(end)
      val coarseWideRight = inside.top.coarseWideRight(begin)
      val coarseWideLeft = inside.top.coarseWideLeft(end)

      updateOutsideUnaries(outside, inside, anchoring, begin, end)
      if(span > 1) {
        val enteredBot = inside.bot.enteredLabelIndexes(begin, end)
        var a = 0
        while(a < grammar.labelIndex.size) {
          if (enteredBot.contains(a)) {
            // a ->  bc  [begin, split, end)
            val rules = anchoring.grammar.indexedBinaryRulesWithParent(a)

            var br = 0
            while(br < rules.length) {
              val r = rules(br)
              val b = grammar.leftChild(r)
              val c = grammar.rightChild(r)
              br += 1

              // can I possibly build any refinement of this rule?
              val narrowR = coarseNarrowRight(b)
              val narrowL = coarseNarrowLeft(c)
              val coarseSplitBegin = math.max(narrowR, coarseWideLeft(c))
              val coarseSplitEnd = math.min(coarseWideRight(b), narrowL) + 1
              val canBuildThisRule = narrowR < end && narrowL >= narrowR && coarseSplitBegin <= narrowL && coarseSplitBegin < coarseSplitEnd

              if(canBuildThisRule) {
                // initialize core scores
                ChartMarginal.fillCoreScores(coreScoreArray,
                  begin, end,
                  anchoring.core,
                  coarseSplitBegin, coarseSplitEnd,
                  r,
                  inside)

                val enteredRefBot = inside.bot.enteredLabelRefinements(begin, end, a)
                val compatibleRefinements = refined.validParentRefinementsGivenRule(begin, coarseSplitBegin, coarseSplitEnd, end, r)

                var refai = 0
                while (refai < compatibleRefinements.length) {
                  val refA = compatibleRefinements(refai)
                  if (enteredRefBot.contains(refA)) {
                    val coreScore = core.scoreSpan(begin, end, a)
                    val aScore:Double = outside.bot.labelScore(begin, end, a, refA) + refined.scoreSpan(begin, end, a, refA) + coreScore
                    if (!aScore.isInfinite) {
                      val ruleRefinements = refined.validRuleRefinementsGivenParent(begin, end, r, refA)
                      var rfI = 0
                      while(rfI < ruleRefinements.length) {
                        val refR = ruleRefinements(rfI)
                        rfI += 1
                        val refB = refined.leftChildRefinement(r, refR)
                        val refC = refined.rightChildRefinement(r, refR)

                        val narrowR = narrowRight(b)(refB)
                        val narrowL = narrowLeft(c)(refC)
                        var split = math.max(narrowR, wideLeft(c)(refC))
                        val endSplit = math.min(wideRight(b)(refB), narrowL) + 1
                        val canBuildThisRule = narrowR < end && narrowL >= narrowR && split <= narrowL && split < endSplit
                        if(!canBuildThisRule)
                          split = endSplit

                        while(split < endSplit) {
                          val bInside = itop.labelScore(begin, split, b, refB)
                          val cInside = itop.labelScore(split, end, c, refC)
                          if (bInside != Double.NegativeInfinity && cInside != Double.NegativeInfinity && aScore != Double.NegativeInfinity) {
                            val ruleScore = refined.scoreBinaryRule(begin, split, end, r, refR) + coreScoreArray(split)
                            val score = aScore + ruleScore
                            val bOutside = score + cInside
                            val cOutside = score + bInside
                            outside.top.rawEnter(begin, split, b, refB, bOutside)
                            outside.top.rawEnter(split, end, c, refC, cOutside)
                          }

                          split += 1
                        }
                      }
                    }
                  }
                  refai += 1
                }
              }
            }
          }

          a += 1
        }
      }
    }
    outside
  }


  private def updateInsideUnaries[L, W](chart: ParseChart[L],
                                        anchoring: AugmentedAnchoring[L, W],
                                        begin: Int, end: Int) = {
    val refined = anchoring.refined
    val core = anchoring.core
    val grammar = anchoring.grammar
    for(bi <- chart.bot.enteredLabelIndexes(begin, end); refB <- chart.bot.enteredLabelRefinements(begin, end, bi)) {
      val b = bi
      val bScore = chart.bot.labelScore(begin, end, b, refB)
      val rules = grammar.indexedUnaryRulesWithChild(b)
      var j = 0
      while(j < rules.length) {
        val r = rules(j)
        val coreScore = core.scoreUnaryRule(begin, end, r)
        if(coreScore != Double.NegativeInfinity) {
          val a = grammar.parent(r)
          for(refR <- refined.validUnaryRuleRefinementsGivenChild(begin, end, r, refB)) {
            val refA = refined.parentRefinement(r, refR)
            val ruleScore: Double = refined.scoreUnaryRule(begin, end, r, refR) + coreScore
            val prob: Double = bScore + ruleScore
            if(prob != Double.NegativeInfinity) {
              chart.top.enter(begin, end, a, refA, prob)
            }
          }
        }
        j += 1
      }
    }

  }

  private def updateOutsideUnaries[L, W](chart: ParseChart[L],
                                         inside: ParseChart[L],
                                         anchoring: AugmentedAnchoring[L, W],
                                         begin: Int, end: Int) = {
    val refined = anchoring.refined
    val core = anchoring.core
    val grammar = anchoring.grammar
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
                chart.bot.rawEnter(begin, end, b, refB, prob)
              }
            }
        }
        j += 1
      }
    }

  }

  private def fillCoreScores[L, W](coreScores: Array[Double],
                                   begin: Int, end: Int,
                                   anchoring: CoreAnchoring[L, W],
                                   splitBegin: Int, splitEnd: Int,
                                   rule: Int,
                                   chart: ParseChart[L]) = {
    var split = splitBegin
    while (split < splitEnd) {
      coreScores(split) = anchoring.scoreBinaryRule(begin, split, end, rule)

      split += 1
    }
  }
}
