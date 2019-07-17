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
import breeze.collection.mutable.TriangularArray
import breeze.linalg.{Counter, DenseVector}

/**
 * Used for computed the expected number of anchored labels that occur at each span
 * @author dlwh
 */
@SerialVersionUID(2L)
class AnchoredSpanProjector(threshold: Double = Double.NegativeInfinity) extends Serializable {

  /**
   * Projects a [[epic.parser.ParseMarginal]] to marginals on anchored rules.
   *
   */
  def projectSpanPosteriors[L, W](charts: ParseMarginal[L, W],
                                  goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):AnchoredSpanProjector.AnchoredData = {

    val length = charts.length
    import charts.topology
    val notAConstituent = topology.labelIndex.size

    def labelBeliefs = {
      DenseVector.zeros[Double](topology.labelIndex.size)
    }

    // The data, and initialization. most things init'd to null
    val totals = TriangularArray.fill[DenseVector[Double]](length+1)(labelBeliefs)
    val totalsUnaries = TriangularArray.fill[DenseVector[Double]](length+1)(labelBeliefs)

    val visitor = new AnchoredVisitor[L] {
      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double): Unit = {
        // fill in spans with 0 if they're active
        if (score > 0.0) {
          totals(begin, end)(tag) += score
        }
      }

      override def skipBinaryRules: Boolean = true

      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, count: Double): Unit = ()

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, count: Double): Unit = {
        if (count > 0.0)
          totalsUnaries(begin, end)(charts.topology.parent(rule)) += count
      }

    }

    charts.visitPostorder(visitor, threshold)

    new AnchoredSpanProjector.AnchoredData(totalsUnaries, totals)
  }
}

object AnchoredSpanProjector {

  /**
   * POJO for anchored rule counts. entries may be null.
   */
  case class AnchoredData(topType: TriangularArray[DenseVector[Double]],
                          botType: TriangularArray[DenseVector[Double]]) {
    def decode[L](ruleTopology: RuleTopology[L]): (TriangularArray[Counter[L, Double]], TriangularArray[Counter[L, Double]]) = {
      topType.map(ruleTopology.labelEncoder.decode(_)) -> botType.map(ruleTopology.labelEncoder.decode(_))
    }
  }

}
