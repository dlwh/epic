package scalanlp.parser
package lex

import scalanlp.collection.mutable.TriangularArray
import scalanlp.tensor.sparse.OldSparseVector
import projections.{AnchoredRuleScorer, AnchoredRuleProjector, GrammarProjections}

/**
 * Used for computed the expected number of anchored rules that occur at each span/split.
 * @author dlwh
 */
@SerialVersionUID(2L)
class LexRuleProjector[C,L,W](parser: LexChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                              indexedProjections: GrammarProjections[C,L], threshold: Double) extends Serializable {

  private def normalize(ruleScores: OldSparseVector):OldSparseVector = {
    if(ruleScores eq null) null
    else {
      val r = new OldSparseVector(ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule,score) <- ruleScores.pairsIterator) {
        val parent = indexedProjections.labels.coarseIndex(indexedProjections.rules.coarseIndex.get(rule).parent)
        r(rule) = math.log(score)
        //        r(rule) = score - totals(parent)
      }
      r
    }
  }

  def createSpanScorer(charts: LexChartPair[ParseChart, L, W]):AnchoredRuleScorer[C] = {
    createSpanScorer(projectRulePosteriors(charts))
  }

  def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData): AnchoredRuleScorer[C] = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores, _, binaryScores, _) = ruleData;
    val normUnaries:Array[OldSparseVector] = for(ruleScores <- unaryScores) yield {
      normalize(ruleScores)
    }

    val normBinaries:Array[Array[OldSparseVector]] = for (splits <- binaryScores) yield {
      if(splits eq null) null
      else for(ruleScores <- splits) yield normalize(ruleScores)
    }
    new AnchoredRuleScorer(lexicalScores, normUnaries, normBinaries);
  }

  /**
   * Projects an inside and outside chart to anchored rule posteriors.
   *
   * @param inside inside chart
   * @param outside outside chart
   * @param sentProb log probability of the root. probably a log partition
   * @param scorer: scorer used to produce this tree.
   * @param pruneLabel should return a threshold to determine if we need to prune. (prune if posterior <= threshold) See companion object for good choices.
   */
  def projectRulePosteriors(charts: LexChartPair[ParseChart, L, W],
                            goldTagPolicy: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags[C]):AnchoredRuleProjector.AnchoredData = {

    val LexChartPair(spec,inside,outside,sentProb,scorer) = charts


    // preliminaries: we're not going to fill in everything: some things will be null.
    // all of this is how to deal with it.
    val numProjectedLabels = indexedProjections.labels.coarseIndex.size;
    val numProjectedRules = indexedProjections.rules.coarseIndex.size;
    def projFill[T>:Null<:AnyRef:ClassManifest]() = Array.fill[T](numProjectedLabels)(null);
    def projVector() = {
      new OldSparseVector(numProjectedLabels, 0.0);
    }

    def projRuleVector() = {
      new OldSparseVector(numProjectedRules, 0.0);
    }

    def getOrElseUpdate[T<:AnyRef](arr: Array[T], i: Int, t : =>T) = {
      if(arr(i) == null) {
        arr(i) = t;
      }
      arr(i);
    }

    // The data, and initialization. most things init'd to null
    val lexicalScores = TriangularArray.raw(inside.length+1,null:OldSparseVector)
    val unaryScores = TriangularArray.raw(inside.length+1,null:OldSparseVector);

    val totals = TriangularArray.raw(inside.length+1,null:OldSparseVector);
    val totalsUnaries = TriangularArray.raw(inside.length+1,null:OldSparseVector);

    val binaryScores = TriangularArray.raw[Array[OldSparseVector]](inside.length+1,null);
    for(begin <- 0 until inside.length; end <- (begin + 1) to inside.length) {
      val numSplits = end - begin;
      if(!inside.bot.enteredLabelIndexes(begin,end).isEmpty) // is there anything to put here?
        binaryScores(TriangularArray.index(begin,end)) = Array.fill(numSplits)(null:OldSparseVector)
    }

    import parser.grammar

    // fill in spans
    for(begin <- 0 until inside.length; end <- (begin + 1) to (inside.length);
        lh <- inside.bot.enteredLabelIndexes(begin,end)) {
      val currentScore = inside.bot.labelScore(begin,end,lh) + outside.bot.labelScore(begin,end,lh) - sentProb;
      val l = inside.bot.decodeLabelPart(lh)
      val pL = indexedProjections.labels.project(l)
      if(currentScore > threshold || goldTagPolicy.isGoldTag(begin,end,pL)) {
        getOrElseUpdate(lexicalScores,TriangularArray.index(begin,end),projVector())(pL) = 0
      }
    }


    // the main loop, similar to cky
    for(diff <- 1 to inside.length) {
      for(begin <- 0 until (inside.length - diff + 1)) {
        val end = begin + diff;
        val index = TriangularArray.index(begin,end);
        val narrowRight = inside.top.narrowRight(begin)
        val narrowLeft = inside.top.narrowLeft(end)
        val wideRight = inside.top.wideRight(begin)
        val wideLeft = inside.top.wideLeft(end)

        // do binaries
        for( ph <- inside.bot.enteredLabelIndexes(begin,end)) {
          val parent = inside.bot.decodeLabelPart(ph)
          val parentScore = outside.bot.labelScore(begin,end,ph)+ scorer.scoreSpan(begin,end,parent);
          val h = inside.bot.decodeHeadPart(ph)
          val pP = indexedProjections.labels.project(parent);

          var total = 0.0

          if(parentScore + inside.bot.labelScore(begin,end,ph) - sentProb > threshold || goldTagPolicy.isGoldTag(begin,end,pP)) {
            val rules = grammar.indexedBinaryRulesWithParent(parent)
            var ruleIndex = 0
            while(ruleIndex < rules.length) {
              val r = rules(ruleIndex)
              val b = grammar.leftChild(r)
              val c = grammar.rightChild(r)
              val pR = indexedProjections.rules.project(r)
              ruleIndex += 1
              // Left child is head: extend right
              if(grammar.isLeftRule(r)) {
                var right = h+1;
                while(right < end)  {

                  val narrowR = narrowRight(inside.top.encodeLabelPair(b, h))
                  val narrowL = narrowLeft(inside.top.encodeLabelPair(c, right))

                  val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                    0L
                  } else {
                    val trueX = wideLeft(inside.top.encodeLabelPair(c, right))
                    val trueMin = if(narrowR > trueX) narrowR else trueX
                    val wr = wideRight(inside.top.encodeLabelPair(b, h))
                    val trueMax = if(wr < narrowL) wr else narrowL
                    if(trueMin > narrowL || trueMin > trueMax)  0L
                    else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
                  }
                  var split = (feasibleSpan >> 32).toInt
                  val endSplit = feasibleSpan.toInt // lower 32 bits
                  if(split < endSplit) {
                    val ruleScore = spec.scoreRightComplement(r, h, right)
                    while(split < endSplit) {
                      // P(sAt->sBu uCt | sAt) \propto \sum_{latent} O(A-x,s,t) r(A-x ->B-y C-z) I(B-y,s,u) I(C-z, s, u)
                      val bScore = inside.top.labelScore(begin,split,inside.top.encodeLabelPair(b, h))
                      val cScore = inside.top.labelScore(split, end, inside.top.encodeLabelPair(c, right))

                      if(bScore != Double.NegativeInfinity && cScore != Double.NegativeInfinity) {
                        val currentScore = (bScore + cScore
                          + parentScore
                          + ruleScore
                          + scorer.scoreBinaryRule(begin,split,end,r)
                          - sentProb);
                        if(currentScore > Double.NegativeInfinity) {
                          val parentArray = if(binaryScores(index)(split-begin) eq null) {
                            binaryScores(index)(split-begin) = projRuleVector()
                            binaryScores(index)(split-begin)
                          } else {
                            binaryScores(index)(split-begin)
                          }
                          val count = math.exp(currentScore)
                          if(count > 0.0) {
                            parentArray(pR) += count
                            total += count
                          }
                          assert(count >= 0,pP)
                          //                    parentArray(pR) = Numerics.logSum(parentArray(pR),currentScore)
                          //                    total = Numerics.logSum(total,currentScore)
                        }
                      }

                      split += 1
                    }
                  }

                  right += 1
                }
              } // end left rule

              if(grammar.isRightRule(r)) {
                var left = begin
                while(left < h) {
                  val narrowR = narrowRight(inside.top.encodeLabelPair(b, left))
                  val narrowL = narrowLeft(inside.top.encodeLabelPair(c, h))

                  val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                    0L
                  } else {
                    val trueX = wideLeft(inside.top.encodeLabelPair(c, h))
                    val trueMin = if(narrowR > trueX) narrowR else trueX
                    val wr = wideRight(inside.top.encodeLabelPair(b, left))
                    val trueMax = if(wr < narrowL) wr else narrowL
                    if(trueMin > narrowL || trueMin > trueMax)  0L
                    else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
                  }
                  var split = (feasibleSpan >> 32).toInt
                  val endSplit = feasibleSpan.toInt // lower 32 bits
                  if(split < endSplit) {
                    val ruleScore = spec.scoreLeftComplement(r, h, left)
                    while(split < endSplit) {
                      // P(sAt->sBu uCt | sAt) \propto \sum_{latent} O(A-x,s,t) r(A-x ->B-y C-z) I(B-y,s,u) I(C-z, s, u)
                      val bScore = inside.top.labelScore(begin,split,inside.top.encodeLabelPair(b, left))
                      val cScore = inside.top.labelScore(split, end, inside.top.encodeLabelPair(c, h))

                      if(bScore != Double.NegativeInfinity && cScore != Double.NegativeInfinity) {
                        val currentScore = (bScore + cScore
                          + parentScore
                          + ruleScore
                          + scorer.scoreBinaryRule(begin,split,end,r)
                          - sentProb);
                        if(currentScore > Double.NegativeInfinity) {
                          val parentArray = if(binaryScores(index)(split-begin) eq null) {
                            binaryScores(index)(split-begin) = projRuleVector()
                            binaryScores(index)(split-begin)
                          } else {
                            binaryScores(index)(split-begin)
                          }
                          val count = math.exp(currentScore)
                          if(count > 0.0) {
                            parentArray(pR) += count
                            total += count
                          }
                          assert(count >= 0,pP)
                          //                    parentArray(pR) = Numerics.logSum(parentArray(pR),currentScore)
                          //                    total = Numerics.logSum(total,currentScore)
                        }
                      }

                      split += 1
                    }
                  }
                  left += 1
                }
              } // end right rule
            }
          }

          if(total != 0.0) {
            if(totals(index) eq null) {
              totals(index) = projVector;
            }
            totals(index)(pP) += total
            assert(total >= 0,pP)
          }
        }

        // do unaries. Similar to above
        for( ph <- outside.top.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.top.labelScore(begin,end,ph);
          val parent = inside.bot.decodeLabelPart(ph)
          val h = inside.bot.decodeHeadPart(ph)
          val pP = indexedProjections.labels.project(parent);

          var total = 0.0
          var parentArray: OldSparseVector = null

          for(r <- parser.grammar.indexedUnaryRulesWithParent(parent)) {
            val c = parser.grammar.child(r)
            val pR = indexedProjections.rules.project(r)
            val ruleScore = spec.scoreUnary(r, h)
            val score = ruleScore + inside.bot.labelScore(begin,end,c,h) + parentScore +
                    scorer.scoreUnaryRule(begin,end,r) - sentProb;
            if(score > Double.NegativeInfinity) {
              if(parentArray eq null)
               parentArray = if(unaryScores(index) eq null) {
                 unaryScores(index) = projRuleVector()
                 unaryScores(index)
               } else {
                 unaryScores(index)
               }
              val count = math.exp(score)
              parentArray(pR) += count
              total += count
            }
          }

          if(total != 0.0) {
            if(totalsUnaries(index) eq null) {
              totalsUnaries(index) = projVector;
            }
            totalsUnaries(index)(pP) += total
          }

        }


      }

    }
    new AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totals);
  }
}

