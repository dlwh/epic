package scalanlp.parser
package projections

import scalanlp.collection.mutable.{OpenAddressHashArray, TriangularArray}

/**
 * Used for computed the expected number of anchored rules that occur at each span/split.
 * @author dlwh
 */
@SerialVersionUID(2L)
class AnchoredRuleProjector(threshold: Double) extends Serializable {

  /**
   * Projects an inside and outside chart to anchored rule posteriors.
   *
   * @param inside inside chart
   * @param outside outside chart
   * @param sentProb log probability of the root. probably a log partition
   * @param scorer: scorer used to produce this tree.
   * @param pruneLabel should return a threshold to determine if we need to prune. (prune if posterior <= threshold) See companion object for good choices.
   */
  def projectRulePosteriors[L,W](charts: Marginal[L, W],
                                 goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):AnchoredRuleProjector.AnchoredData = {

    val length = charts.length
    // preliminaries: we're not going to fill in everything: some things will be null.
    // all of this is how to deal with it.
    val numProjectedLabels = charts.grammar.labelIndex.size
    val numProjectedRules = charts.grammar.index.size
    def projVector() = {
      new OpenAddressHashArray(numProjectedLabels, 0.0, 2);
    }

    def projRuleVector() = {
      new OpenAddressHashArray(numProjectedRules, 0.0, 2);
    }

    def getOrElseUpdate[T<:AnyRef](arr: Array[T], i: Int, t : =>T) = {
      if(arr(i) == null) {
        arr(i) = t;
      }
      arr(i);
    }

    // The data, and initialization. most things init'd to null
    val lexicalScores = TriangularArray.raw(length+1, null:OpenAddressHashArray[Double])
    val unaryScores = TriangularArray.raw(length+1, null:OpenAddressHashArray[Double]);
    val binaryScores = TriangularArray.raw[Array[OpenAddressHashArray[Double]]](length+1, null);

    val totals = TriangularArray.raw(length+1, null:OpenAddressHashArray[Double]);
    val totalsUnaries = TriangularArray.raw(length+1, null:OpenAddressHashArray[Double]);


    val visitor = new AnchoredVisitor[L] {
      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        // fill in spans with 0 if they're active
        getOrElseUpdate(lexicalScores, TriangularArray.index(begin, end), projVector())(tag) = 0
      }

      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, count: Double) {
        val index = TriangularArray.index(begin, end)
        if(count > 0.0) {
          if(totals(index) eq null) {
            totals(index) = projVector()
          }
          totals(index)(charts.grammar.parent(rule)) += count

          if(binaryScores(index) eq null) {
            val numSplits = end - begin;
            binaryScores(TriangularArray.index(begin, end)) = Array.fill(numSplits)(null:OpenAddressHashArray[Double])
          }

          val parentArray = if(binaryScores(index)(split-begin) eq null) {
            binaryScores(index)(split-begin) = projRuleVector()
            binaryScores(index)(split-begin)
          } else {
            binaryScores(index)(split-begin)
          }
          parentArray(rule) += count
        }
      }

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, count: Double) {
        val index = TriangularArray.index(begin, end)
        val parentArray = if(unaryScores(index) eq null) {
          unaryScores(index) = projRuleVector()
          unaryScores(index)
        } else {
          unaryScores(index)
        }
        parentArray(rule) += count
        if(totalsUnaries(index) eq null) {
          totalsUnaries(index) = projVector()
        }
        totalsUnaries(index)(charts.grammar.parent(rule)) += count
      }

    }

    charts.visit(visitor)

    new AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totals);
  }
}


object AnchoredRuleProjector {

  /**
   * POJO for anchored rule counts. entries may be null.
   */
  case class AnchoredData(/** spanScore(trianuglarIndex)(label) = score of tag at position pos */
                          spanScores: Array[OpenAddressHashArray[Double]],
                          /** unaryScores(triangularIndex)(rule) => score of unary from parent to child */
                          unaryScores: Array[OpenAddressHashArray[Double]],
                          /** (triangularIndex)(parent) => same, but for unaries*/
                          unaryTotals: Array[OpenAddressHashArray[Double]],
                          /** binaryScores(triangularIndex)(split)(rule) => score of unary from parent to child */
                          binaryScores: Array[Array[OpenAddressHashArray[Double]]],
                          /** (triangularIndex)(parent) => sum of all binary rules at parent. */
                          binaryTotals: Array[OpenAddressHashArray[Double]]);

}


