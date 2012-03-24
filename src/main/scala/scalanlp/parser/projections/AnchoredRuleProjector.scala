package scalanlp.parser
package projections

import scalala.library.Numerics
import scalanlp.trees.BinarizedTree
import sun.misc.Unsafe
import scalanlp.collection.mutable.{OpenAddressHashArray, TriangularArray}
import scalanlp.tensor.sparse.OldSparseVector

/**
 * Used for computed the expected number of anchored rules that occur at each span/split.
 * @author dlwh
 */
@SerialVersionUID(2L)
class AnchoredRuleProjector[C,L,W](coarseGrammar: Grammar[C],
                                   parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                   indexedProjections: GrammarProjections[C,L], threshold: Double) extends Serializable {


  /**
   * Projects an inside and outside chart to anchored rule posteriors.
   *
   * @param inside inside chart
   * @param outside outside chart
   * @param sentProb log probability of the root. probably a log partition
   * @param scorer: scorer used to produce this tree.
   * @param pruneLabel should return a threshold to determine if we need to prune. (prune if posterior <= threshold) See companion object for good choices.
   */
  def projectRulePosteriors(charts: ChartPair[ParseChart, L],
                            goldTagPolicy: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags[C]):AnchoredRuleProjector.AnchoredData = {

    val ChartPair(inside,outside,sentProb,scorer) = charts


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


    import parser.grammar;

    // fill in spans
    for(begin <- 0 until inside.length; end <- (begin + 1) to (inside.length);
        l <- inside.bot.enteredLabelIndexes(begin,end)) {
      val currentScore = inside.bot.labelScore(begin,end,l) + outside.bot.labelScore(begin,end,l) - sentProb;
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
        for( parent <- inside.bot.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.bot.labelScore(begin,end,parent)+ scorer.scoreSpan(begin,end,parent);
          val pP = indexedProjections.labels.project(parent);

          var total = 0.0

          if(parentScore + inside.bot.labelScore(begin,end,parent) - sentProb > threshold || goldTagPolicy.isGoldTag(begin,end,pP)) {
            val rules = grammar.indexedBinaryRulesWithParent(parent)
            var ruleIndex = 0
            while(ruleIndex < rules.length) {
              val r = rules(ruleIndex)
              val b = grammar.leftChild(r)
              val c = grammar.rightChild(r)
              val ruleScore = grammar.ruleScore(r)
              val pR = indexedProjections.rules.project(r)
              ruleIndex += 1

              // this is too slow, so i'm having to inline it.
//              val feasibleSpan = itop.feasibleSpanX(begin, end, b, c)
              val narrowR = narrowRight(b)
              val narrowL = narrowLeft(c)

              val feasibleSpan = if (narrowR >= end || narrowL < narrowR) {
                0L
              } else {
                val trueX = wideLeft(c)
                val trueMin = if(narrowR > trueX) narrowR else trueX
                val wr = wideRight(b)
                val trueMax = if(wr < narrowL) wr else narrowL
                if(trueMin > narrowL || trueMin > trueMax)  0L
                else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
              }
              var split = (feasibleSpan >> 32).toInt
              val endSplit = feasibleSpan.toInt // lower 32 bits
              while(split < endSplit) {
                // P(sAt->sBu uCt | sAt) \propto \sum_{latent} O(A-x,s,t) r(A-x ->B-y C-z) I(B-y,s,u) I(C-z, s, u)
                val bScore = inside.top.labelScore(begin,split,b)
                val cScore = inside.top.labelScore(split, end, c)

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
        for( parent <- outside.top.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.top.labelScore(begin,end,parent);
          val pP = indexedProjections.labels.project(parent);

          var total = 0.0
          var parentArray: OldSparseVector = null

          for(r <- parser.grammar.indexedUnaryRulesWithParent(parent)) {
            val c = parser.grammar.child(r)
            val pR = indexedProjections.rules.project(r)
            val ruleScore = parser.grammar.ruleScore(r)
            val score = ruleScore + inside.bot.labelScore(begin,end,c) + parentScore +
                    scorer.scoreUnaryRule(begin,end,r) - sentProb;
            val pC = indexedProjections.labels.project(c)
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
//              parentArray(pR) = Numerics.logSum(parentArray(pR),score)
//              total = Numerics.logSum(total,score)
              total += count
            }
          }

          if(total != 0.0) {
            if(totalsUnaries(index) eq null) {
              totalsUnaries(index) = projVector;
            }
//            totalsUnaries(index)(pP) = Numerics.logSum(totalsUnaries(index)(pP),total)
            totalsUnaries(index)(pP) += total
          }

        }


      }

    }
    new AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totals);
  }
}


object AnchoredRuleProjector {

  /**
   * POJO for anchored rule counts. entries may be null.
   */
  case class AnchoredData(/** spanScore(trianuglarIndex)(label) = score of tag at position pos */
                          spanScores: Array[OldSparseVector],
                          /** unaryScores(triangularIndex)(rule) => score of unary from parent to child */
                          unaryScores: Array[OldSparseVector],
                          /** (triangularIndex)(parent) => same, but for unaries*/
                          unaryTotals: Array[OldSparseVector],
                          /** binaryScores(triangularIndex)(split)(rule) => score of unary from parent to child */
                          binaryScores: Array[Array[OldSparseVector]],
                          /** (triangularIndex)(parent) => sum of all binary rules at parent. */
                          binaryTotals: Array[OldSparseVector]);

}


