package epic.parser
package projections

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
import breeze.collection.mutable.{TriangularArray, OpenAddressHashArray}

/**
 * Used for computing the x-bar parse forest from a [[epic.parser.ParseMarginal]].
 * @author dlwh
 */
@SerialVersionUID(2L)
class AnchoredForestProjector(threshold: Double) extends Serializable {

  /**
   * Projects a [[epic.parser.ParseMarginal]] to marginals on anchored rules.
   *
   */
  def projectRulePosteriors[L, W](charts: ParseMarginal[L, W],
                                  goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):AnchoredForestProjector.ForestData = {

    val length = charts.length
    // preliminaries: we're not going to fill in everything: some things will be null.
    // all of this is how to deal with it.
    val numProjectedLabels = charts.topology.labelIndex.size
    val numProjectedRules = charts.topology.index.size
    def projVector() = {
      new OpenAddressHashArray(numProjectedLabels, 0.0)
    }

    def projRuleVector() = {
      new OpenAddressHashArray(numProjectedRules, 0.0)
    }

    def getOrElseUpdate[T<:AnyRef](arr: Array[T], i: Int, t : =>T) = {
      if (arr(i) == null) {
        arr(i) = t
      }
      arr(i)
    }

    // The data, and initialization. most things init'd to null
    val lexicalScores = TriangularArray.raw(length+1, null:OpenAddressHashArray[Double])
    val unaryScores = TriangularArray.raw(length+1, null:OpenAddressHashArray[Double])
    val binaryScores = TriangularArray.raw[Array[OpenAddressHashArray[Double]]](length+1, null)

    val totals = TriangularArray.raw(length+1, null:OpenAddressHashArray[Double])
    val totalsUnaries = TriangularArray.raw(length+1, null:OpenAddressHashArray[Double])

    val visitor = new AnchoredVisitor[L] {
      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        // fill in spans with 0 if they're active
        if (score > 0.0) {
          val index = TriangularArray.index(begin, end)
          getOrElseUpdate(lexicalScores, index, projVector())(tag) = 1.0
          if (totals(index) eq null) {
            totals(index) = projVector()
          }
          totals(index)(tag) += score
        }
      }

      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, count: Double) {
        if (count > 0.0) {
          val index = TriangularArray.index(begin, end)
          var forSpan = binaryScores(index)
          if (forSpan eq null) {
            val numSplits = end - begin
            forSpan = new Array[OpenAddressHashArray[Double]](numSplits)
            binaryScores(index) = forSpan
          }

          val parentArray = if (forSpan(split-begin) eq null) {
            forSpan(split-begin) = projRuleVector()
            forSpan(split-begin)
          } else {
            forSpan(split-begin)
          }
          parentArray(rule) += count
        }
      }

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, count: Double) {
        val index = TriangularArray.index(begin, end)
        val parentArray = if (unaryScores(index) eq null) {
          unaryScores(index) = projRuleVector()
          unaryScores(index)
        } else {
          unaryScores(index)
        }
        parentArray(rule) += count
        if (totalsUnaries(index) eq null) {
          totalsUnaries(index) = projVector()
        }
        totalsUnaries(index)(charts.topology.parent(rule)) += count
      }

    }

    charts.visitPostorder(visitor, threshold)

    new AnchoredForestProjector.ForestData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totals)
  }
}

object AnchoredForestProjector {

  /**
   * POJO for anchored rule counts. entries may be null.
   */
  case class ForestData(/** spanScore(trianuglarIndex)(label) = score of tag at position pos */
                        spanScores: Array[OpenAddressHashArray[Double]],
                        /** unaryScores(triangularIndex)(rule) => score of unary from parent to child */
                        unaryScores: Array[OpenAddressHashArray[Double]],
                        /** (triangularIndex)(parent) => same, but for unaries*/
                        unaryTotals: Array[OpenAddressHashArray[Double]],
                        /** binaryScores(triangularIndex)(split)(rule) => score of unary from parent to child */
                        binaryScores: Array[Array[OpenAddressHashArray[Double]]],
                        /** (triangularIndex)(parent) => sum of all binary rules at parent. */
                        binaryTotals: Array[OpenAddressHashArray[Double]])

}


