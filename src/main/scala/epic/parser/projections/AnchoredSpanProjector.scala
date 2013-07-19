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
import breeze.linalg.DenseVector

/**
 * Used for computed the expected number of anchored rules that occur at each span/split.
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
    import charts.grammar
    val notAConstituent = grammar.labelIndex.size

    def optionalLabelBeliefs = {
      val r = DenseVector.zeros[Double](grammar.labelIndex.size + 1)
      r(notAConstituent) = 1.0
      r
    }

    // The data, and initialization. most things init'd to null
    val totals = TriangularArray.fill[DenseVector[Double]](length+1)(optionalLabelBeliefs)
    val totalsUnaries = TriangularArray.fill[DenseVector[Double]](length+1)(optionalLabelBeliefs)


    val visitor = new AnchoredVisitor[L] {
      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        // fill in spans with 0 if they're active
        if(score > 0.0) {
          siphonMass(totals(begin, end), tag, notAConstituent, score)
        }
      }


      override def skipBinaryRules: Boolean = true

      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, count: Double) {

      }

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, count: Double) {
        if (count > 0.0)
          siphonMass(totalsUnaries(begin, end), charts.grammar.parent(rule), notAConstituent, count)
      }


      /**
       * subtracts score from counts(from) and adds it to counts(to). ensures
       * that counts(from) does not go negative (a little negative is sent to 0, a
       * lot throws.)
       */
      private def siphonMass(counts: DenseVector[Double], to: Int, from: Int, score: Double) {
        counts(to) += score
        counts(from) -= score
        if (counts(from) < 0) {
          assert(counts(from) > -1E-6)
          counts(from) = 0.0
        }
      }

    }


    charts.visitPostorder(visitor, threshold)

    for(i <- 0 until length) {
      val cell = totalsUnaries(i, i + 1)
      assert(cell(notAConstituent).abs < 1E-8, i + " " + cell + " " + charts.words)
      cell(notAConstituent) = 0.0
    }

    new AnchoredSpanProjector.AnchoredData(totalsUnaries, totals)
  }
}






object AnchoredSpanProjector {

  /**
   * POJO for anchored rule counts. entries may be null.
   */
  case class AnchoredData(topType: TriangularArray[DenseVector[Double]],
                          botType: TriangularArray[DenseVector[Double]])

}
