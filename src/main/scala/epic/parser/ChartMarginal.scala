package epic.parser

import epic.trees.{UnaryTree, BinarizedTree}
import epic.util.Arrays
import breeze.numerics
import breeze.collection.mutable.TriangularArray

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
final case class ChartMarginal[L, W](anchoring: AugmentedAnchoring[L, W],
                                     inside: ParseChart[L], outside: ParseChart[L],
                                     logPartition: Double,
                                     isMaxMarginal: Boolean) extends ParseMarginal[L, W] {

  def checkForTree(tree: BinarizedTree[(L, Int)]) = {
    for (t <- tree.allChildren) t match {
      case UnaryTree( (label, ref), _, _, span) =>
        val labelScore = inside.top(span.start, span.end, anchoring.grammar.labelIndex(label), ref)
        if (labelScore.isInfinite) {
          println("problem with unary: " + (label, ref) + " " + span)
        }
      case tree =>
        val labelScore = inside.bot(tree.span.start, tree.span.end, anchoring.grammar.labelIndex(tree.label._1), tree.label._2)
        if (labelScore.isInfinite) {
          println("problem with other: " + t.label + " " + tree.span)
        }
    }
    this
  }

  def checkForSimpleTree(tree: BinarizedTree[L]) = {
    for (t <- tree.allChildren) t match {
      case UnaryTree( label, _, _, span) =>
        val labelScore = breeze.linalg.softmax(inside.top.decodedLabelScores(span.start, span.end, anchoring.grammar.labelIndex(label)))
        if (labelScore.isInfinite) {
          println("problem with unary: " + (label) + " " + span)
        }
      case tree =>
        val labelScore = breeze.linalg.softmax(inside.bot.decodedLabelScores(tree.start, tree.end, anchoring.grammar.labelIndex(tree.label)))
        if (labelScore.isInfinite) {
          println("problem with other: " + t.label + " " + tree.span)
        }
    }
    this
  }


  def checkForTreeOutside(tree: BinarizedTree[(L, Int)]) {
    for (t <- tree.allChildren) t match {
      case tree@UnaryTree( (label, ref), _, _, span) =>
        val ai: Int = anchoring.grammar.labelIndex(label)
        val labelScore = outside.top(span.start, span.end, ai, ref)
        if (labelScore.isInfinite) {
          ChartMarginal.synchronized {
            println("problem with top: " + (label, ref) + " " + span)
            println(s"problem with outside other: ${t.label} ${tree.span} ${outside.top.enteredLabelIndexes(tree.span.start, tree.span.end)(ai)} $words ${outside.top.decodedLabelScores(tree.span.start,tree.span.end)}")
            println(ai + " " + outside.top.enteredLabels(TriangularArray.index(tree.span.start, tree.span.end)))
            println("Constraint: " + anchoring.core.sparsityPattern.top.isAllowedLabeledSpan(tree.start, tree.end, ai))
            println("checking for inside starting from here...")
            checkForTree(t.asInstanceOf[BinarizedTree[(L, Int)]])
            println("done.")
          }
          return
        }
      case tree =>
        val ai: Int = anchoring.grammar.labelIndex(tree.label._1)
        val labelScore = outside.bot(tree.span.start, tree.span.end, ai, tree.label._2)
        if (labelScore.isInfinite) {
          ChartMarginal.synchronized {
            println(s"problem with outside other: ${t.label} ${tree.span} ${outside.bot.enteredLabelIndexes(tree.span.start, tree.span.end)(ai)} $words ${outside.bot.decodedLabelScores(tree.span.start,tree.span.end)}")
            println(ai + " " + outside.bot.enteredLabels(TriangularArray.index(tree.span.start, tree.span.end)))
            println("Constraint: " + anchoring.core.sparsityPattern.bot.isAllowedLabeledSpan(tree.start, tree.end, ai))
            println("checking for inside starting from here...")
            checkForTree(t.asInstanceOf[BinarizedTree[(L, Int)]])
          }
          return
        }
    }
  }

  def verify() {
    assert(!logPartition.isInfinite, anchoring.words)

    val ins = inside.bot.enteredLabelScores(0, length).toMap
    var score = Double.NegativeInfinity
    import breeze.linalg._
    for((sym,scores) <- outside.bot.enteredLabelScores(0, length)) {
      score = numerics.logSum(score, softmax(new DenseVector(scores) + new DenseVector(ins(sym))))
    }
    assert( (score - logPartition).abs/math.max(logPartition.abs,score.abs).max(1E-4) < 1E-4, logPartition + " " + 0 + "Z" + score)
    for(i <- 0 until length) {
      val ins = inside.bot.enteredLabelScores(i, i+1).toMap
      var score = Double.NegativeInfinity
      import breeze.linalg._
      for((sym,scores) <- outside.bot.enteredLabelScores(i, i+1)) {
        score = numerics.logSum(score, softmax(new DenseVector(scores) + new DenseVector(ins(sym))))
      }
      assert( (score - logPartition).abs/math.max(logPartition.abs,score.abs).max(1E-4) < 1E-4, logPartition + " " + i + " " + score)
    }


  }
  //  verify()


  /**
   * Forest traversal that visits spans in a "bottom up" order.
   */
  def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double = Double.NegativeInfinity) {
    if(logPartition.isInfinite) throw new RuntimeException("No parse for " + words)
    val itop = inside.top

    val lexLoc = lexicon.anchor(words)

    // handle lexical
    for (i <- 0 until words.length) {
      for {
        a <- lexLoc.allowedTags(i)
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
      val narrowRight = inside.top.leftMostEndForBegin(begin)
      val narrowLeft = inside.top.rightMostBeginForEnd(end)
      val wideRight = inside.top.rightMostEndForBegin(begin)
      val wideLeft = inside.top.leftMostBeginForEnd(end)

      val coarseNarrowRight = inside.top.coarseLeftMostEndForBegin(begin)
      val coarseNarrowLeft = inside.top.coarseRightMostBeginForEnd(end)
      val coarseWideRight = inside.top.coarseRightMostEndForBegin(begin)
      val coarseWideLeft = inside.top.coarseLeftMostBeginForEnd(end)

      for (a <- inside.bot.enteredLabelIndexes(begin, end); refA <- inside.bot.enteredLabelRefinements(begin, end, a)) {
        var i = 0
        val aOutside = outside.bot.labelScore(begin, end, a, refA)
        val labelMarginal = aOutside + inside.bot.labelScore(begin, end, a, refA) - logPartition
        val aScore = aOutside + anchoring.scoreSpan(begin, end, a, refA)
        if(labelMarginal > spanThreshold) {
          spanVisitor.visitSpan(begin, end, a, refA, math.exp(labelMarginal))
          if(!spanVisitor.skipBinaryRules) {

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
                  r)

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
    }

    // Unaries
    if(!spanVisitor.skipUnaryRules)
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
                                             sent: IndexedSeq[W]): ChartMarginal[L, W] = {
    apply(grammar.anchor(sent), sent)
  }

  def apply[L, W, Chart[X] <: ParseChart[X]](anchoring: AugmentedAnchoring[L, W],
                                             sent: IndexedSeq[W], maxMarginal: Boolean = false): ChartMarginal[L, W] = {
    val sum = if(maxMarginal) MaxSummer else LogSummer
    val (inside, spanScores) = buildInsideChart(anchoring, sent, sum)
    val logPartition = rootScore(anchoring, inside, sum)
    val outside = buildOutsideChart(anchoring, inside, spanScores, sum)
    ChartMarginal(anchoring, inside, outside, logPartition, maxMarginal)
  }

  private trait Summer {
    def apply(a: Double, b: Double):Double
    def apply(a: Array[Double], b: Int):Double
  }

  private object LogSummer extends Summer {
    def apply(a: Double, b: Double): Double = numerics.logSum(a,b)
    def apply(a: Array[Double], b: Int): Double = numerics.logSum(a,b)
  }

  private object MaxSummer extends Summer {
    def apply(a: Double, b: Double): Double = math.max(a,b)
    def apply(a: Array[Double], b: Int): Double = numerics.max(a,b)
  }

  private def rootScore[L, W](anchoring: AugmentedAnchoring[L, W], inside: ParseChart[L], sum: Summer): Double = {
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
    val score = sum(rootScores, offset)
    assert(!score.isNaN, rootScores.mkString(", "))
    score
  }

  // first parse chart is the inside scores, second parse chart is span scores for spans that were computed.
  private def buildInsideChart[L, W, Chart[X] <: ParseChart[X]](anchoring: AugmentedAnchoring[L, W],
                                                                words: IndexedSeq[W], sum: Summer): (ParseChart[L], ParseChart[L]) = {
    val refined = anchoring.refined
    val core = anchoring.core

    val grammar = anchoring.grammar
    val lexicon = anchoring.lexicon

    val inside = ParseChart.logProb.apply(grammar.labelIndex,
      Array.tabulate(grammar.labelIndex.size)(refined.numValidRefinements),
      words.length,
      core.sparsityPattern)
    val spanScores = ParseChart.logProb.apply(grammar.labelIndex,
      Array.tabulate(grammar.labelIndex.size)(refined.numValidRefinements),
      words.length,
      core.sparsityPattern)
    val lexLoc = lexicon.anchor(words)

    // handle lexical
    for{i <- 0 until words.length} {
      assert(core.sparsityPattern.isAllowedSpan(i,i+1))
      assert(core.sparsityPattern.bot.isAllowedSpan(i,i+1))
      var foundSomething = false
      for {
        a <- lexLoc.allowedTags(i)
        coreScore = core.scoreSpan(i, i+1, a) if coreScore != Double.NegativeInfinity
        ref <- refined.validLabelRefinements(i, i+1, a)
      } {
        val score:Double = refined.scoreSpan(i, i+1, a, ref) + coreScore
        if (score != Double.NegativeInfinity) {
          spanScores.bot.enter(i, i+1, a, ref, score)
          inside.bot.enter(i, i+1, a, ref, score)
          foundSomething = true
        }
      }

      updateInsideUnaries(inside, anchoring,  i, i+1, sum)
    }


    // cache for coreScores
    val coreScoreArray = new Array[Double](words.length)

    val top = inside.top
    val g = grammar

    // a -> bc over [begin, split, end)
    for {
      span <- 2 to words.length
      begin:Int <- 0 to (words.length - span)
    } {
      val end = begin + span
      // I get a 20% speedup by inlining code dealing with these arrays. sigh.
      val narrowRight = top.leftMostEndForBegin(begin)
      val narrowLeft = top.rightMostBeginForEnd(end)
      val wideRight = top.rightMostEndForBegin(begin)
      val wideLeft = top.leftMostBeginForEnd(end)

      val coarseNarrowRight = top.coarseLeftMostEndForBegin(begin)
      val coarseNarrowLeft = top.coarseRightMostBeginForEnd(end)
      val coarseWideRight = top.coarseRightMostEndForBegin(begin)
      val coarseWideLeft = top.coarseLeftMostBeginForEnd(end)
      val scoreArray = Arrays.newArray(anchoring.refined.maxLabelRefinements,  40)
      val offsets = new Array[Int](anchoring.refined.maxLabelRefinements)
      val spanScoresEntered = new Array[Boolean](anchoring.refined.maxLabelRefinements)

      for ( a <- 0 until grammar.labelIndex.size ) {
        val numValidLabelRefs = anchoring.refined.numValidRefinements(a)
        java.util.Arrays.fill(offsets, 0)
        java.util.Arrays.fill(spanScoresEntered, false)

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
                r)

              val validA = refined.validParentRefinementsGivenRule(begin, coarseSplitBegin, coarseSplitEnd, end, r)
              var ai = 0
              while(ai < validA.length) {
                val refA = validA(ai)
                ai += 1
                val spanScore = refined.scoreSpan(begin, end, a, refA) + coreSpan
                if(!spanScore.isInfinite) {
                  if(!spanScoresEntered(refA)) {
                    spanScores.bot.enter(begin, end, a, refA, spanScore)
                    spanScoresEntered(refA) = true
                  }

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
                      val withoutRule = bScore + cScore + spanScore + coreScoreArray(split)
                      if(withoutRule != Double.NegativeInfinity) {

                        val prob = withoutRule + refined.scoreBinaryRule(begin, split, end, r, refR)

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
                }

              } // end a refinement



            } // end canBuildThisRule
          } // end rules

          var foundSomething = false
          var ai = 0
          while(ai < numValidLabelRefs) {
            // done updating vector, do an enter:
            if(offsets(ai) > 0) {
              inside.bot.enter(begin, end, a, ai, sum(scoreArray(ai), offsets(ai)))
              foundSomething = true
            }
            ai += 1
          }
//          if(!foundSomething && anchoring.core.sparsityPattern != ChartConstraints.noSparsity) {
//            println(s"Failed to replicate a span in ($begin, $end) of ${anchoring.words}. Label is ${anchoring.grammar.labelIndex.get(a)}")
//
//          }

        }
      }
      updateInsideUnaries(inside, anchoring, begin, end, sum)
    }
    inside -> spanScores
  }

  private def buildOutsideChart[L, W](anchoring: AugmentedAnchoring[L, W],
                                      inside: ParseChart[L], spanScores: ParseChart[L], sum: Summer):ParseChart[L] = {
    val refined = anchoring.refined
    val core = anchoring.core

    val grammar = anchoring.grammar
    val rootIndex = grammar.labelIndex(grammar.root)

    // cache for coreScores
    val coreScoreArray = new Array[Double](inside.length + 1)

    val length = inside.length
    val outside = ParseChart.logProb.apply(grammar.labelIndex,
      Array.tabulate(grammar.labelIndex.size)(refined.numValidRefinements),
      length,
      core.sparsityPattern)
    for(refRoot <- refined.validLabelRefinements(0, inside.length, rootIndex)) {
      outside.top.enter(0, inside.length, rootIndex, refRoot, 0.0)
    }
    updateOutsideUnaries(outside, inside, anchoring, 0, inside.length, sum)

    val scoreArray = Arrays.newArray(anchoring.refined.maxLabelRefinements,  80)
    val offsets = new Array[Int](anchoring.refined.maxLabelRefinements)

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

          val numValidLabelRefs = anchoring.refined.numValidRefinements(a)
          var ai = 0
          while(ai < numValidLabelRefs) {
            // done updating vector, do an enter:
            if(offsets(ai) > 0) {
//              println(s"$begin, $end) ${grammar.labelIndex.get(a)} $ai ${offsets(ai)}")
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


  private def doOutsideLeftCompletionUpdates[W, L](inside: ParseChart[L], outside: ParseChart[L],
                                                   spanScores: ParseChart[L],
                                                   anchoring: AugmentedAnchoring[L, W],
                                                   begin: Int, end: Int,
                                                   label: Int,
                                                   coreScoreArray: Array[Double],
                                                   scoreArray: Array[Array[Double]], offsets: Array[Int], sum: Summer) {
    val refined = anchoring.refined
    val itop = inside.top
    val grammar = refined.grammar
    val rules = anchoring.grammar.indexedBinaryRulesWithLeftChild(label)
    val length = inside.length

//    println(grammar.labelIndex.get(label) + " " + rules.mkString(", "))

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

//      println(coarseCompletionBegin + " " + coarseCompletionEnd)

      if (canBuildThisRule) {
        // initialize core scores
        ChartMarginal.fillCoreScoresForLeftCompletion(coreScoreArray,
          begin, end,
          anchoring.core,
          coarseCompletionBegin, coarseCompletionEnd,
          r)


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

  private def doOutsideRightCompletionUpdates[W, L](inside: ParseChart[L], outside: ParseChart[L],
                                                    spanScores: ParseChart[L],
                                                    anchoring: AugmentedAnchoring[L, W],
                                                    begin: Int, end: Int,
                                                    label: Int,
                                                    coreScoreArray: Array[Double],
                                                    scoreArray: Array[Array[Double]],
                                                    offsets: Array[Int], sum: Summer) {
    val refined = anchoring.refined
    val itop = inside.top
    val grammar = refined.grammar
    val rules = anchoring.grammar.indexedBinaryRulesWithRightChild(label)

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
        ChartMarginal.fillCoreScoresForRightCompletion(coreScoreArray,
          begin, end,
          anchoring.core,
          coarseCompletionBegin, coarseCompletionEnd,
          r)

//        println(s"building right ${grammar.index.get(r)} on [$coarseCompletionBegin, $coarseCompletionEnd) $begin $end" )

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
//                println("?!?!?")
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

  private def updateInsideUnaries[L, W](chart: ParseChart[L],
                                        anchoring: AugmentedAnchoring[L, W],
                                        begin: Int, end: Int, sum: Summer) = {
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
              chart.top.enter(begin, end, a, refA, sum(chart.top.labelScore(begin, end, a, refA), prob))
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
                                         begin: Int, end: Int, sum: Summer) = {
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
                chart.bot.enter(begin, end, b, refB, sum(chart.bot.labelScore(begin, end, b, refB), prob))
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
                                   rule: Int) {
    var split = splitBegin
    while (split < splitEnd) {
      coreScores(split) = anchoring.scoreBinaryRule(begin, split, end, rule)

      split += 1
    }
  }

  private def fillCoreScoresForLeftCompletion[L, W](coreScores: Array[Double],
                                                    begin: Int, end: Int,
                                                    anchoring: CoreAnchoring[L, W],
                                                    completionBegin: Int, completionEnd: Int,
                                                    rule: Int) {
    var completion = completionBegin
    while (completion <= completionEnd) {
      coreScores(completion) = anchoring.scoreBinaryRule(begin, end, completion, rule)

      completion += 1
    }
  }

  private def fillCoreScoresForRightCompletion[L, W](coreScores: Array[Double],
                                                     begin: Int, end: Int,
                                                     anchoring: CoreAnchoring[L, W],
                                                     completionBegin: Int, completionEnd: Int,
                                                     rule: Int) {
    var completion = completionBegin
    while (completion <= completionEnd) {
      coreScores(completion) = anchoring.scoreBinaryRule(completion, begin, end, rule)

      completion += 1
    }
  }



}

