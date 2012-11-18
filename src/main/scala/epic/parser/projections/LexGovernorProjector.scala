package epic.parser
package projections

import breeze.collection.mutable.TriangularArray
import breeze.linalg._
import models.LexGrammar


/**
 * 
 * @author dlwh
 */
class LexGovernorProjector[L, W](grammar: LexGrammar[L, W], notConstituent: L) {
  private val nc = grammar.grammar.labelIndex(notConstituent)

  def apply(chart: Marginal[L, W]):LexGovernorInfo = {
    val v = new Visitor(chart.length)
    chart.visit(v)
    LexGovernorInfo(v.spanType, v.spanGovernorCounts, v.wordGovernorCounts, v.maximalLabelType)
  }

  // WHENEVER you change this, be sure to change PropertyParsingAnchoring
  private class Visitor(length: Int) extends AnchoredVisitor[L] {
    val spanType = TriangularArray.fill(length)(labelBeliefs)
    val spanGovernorCounts = TriangularArray.fill(length)(baseDV)
    val wordGovernorCounts = Array.fill(length)(DenseVector.zeros[Double](length+1))
    val maximalLabelType = Array.fill(length)(labelBeliefs)
    val wordTagType = Array.fill(length)(labelBeliefs)

    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val head = (ref:Int) / length
      val dep = (ref:Int) % length
      wordGovernorCounts(dep)(head) += score
      if (grammar.isRightRule(rule)) { // head on the right
        spanGovernorCounts(begin, split)(head) += score
        spanGovernorCounts(begin, split)(notASpan) -= score
        val label = grammar.grammar.rightChild(rule)
        maximalLabelType(dep)(label) += score
        maximalLabelType(dep)(label) -= score
      } else {
        spanGovernorCounts(split, end)(head) += score
        spanGovernorCounts(split, end)(notASpan) -= score
        val label = grammar.grammar.leftChild(rule)
        maximalLabelType(dep)(label) += score
        maximalLabelType(dep)(label) -= score
      }
    }

    def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val parent = grammar.grammar.parent(rule)
      spanType(begin,end)(parent) += score
      spanType(begin,end)(nc) -= score
    }

    def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
      if (begin == 0 && end == length) { // root, get the length
        wordGovernorCounts(ref)(length) += score
        spanGovernorCounts(begin, end)(length) += score
        spanGovernorCounts(begin, end)(notASpan) -= score
        maximalLabelType(ref)(tag) += score
        maximalLabelType(ref)(tag) -= score
      }

      if(begin + 1 == end) {
        wordTagType(begin)(tag) += score
        wordTagType(begin)(nc) -= score
      }
    }

    def baseDV = {
      val r = DenseVector.zeros[Double](length+2)
      // length is root, length + 1 is "span is off"
      // start with all mass in "is off", and subtract
      r(notASpan) = 1.0
      r
    }

    private val notASpan = length+1
    def labelBeliefs = {
      val r = grammar.grammar.mkDenseVector()
      r(nc) = 1.0
      r
    }
  }
}

/**
 *
 * @param spanType
 * @param spanGovernor which word governs my span. not my head. my head's head. length = root (i.e. whole setnence), length+1 == off
 * @param wordGovernor which word governs me, i.e. head. length = root
 * @param maximalLabelType for each word the type of its maximal projection
 */
case class LexGovernorInfo(spanType: TriangularArray[DenseVector[Double]],
                           spanGovernor: TriangularArray[DenseVector[Double]],
                           wordGovernor: Array[DenseVector[Double]],
                           maximalLabelType: Array[DenseVector[Double]])
