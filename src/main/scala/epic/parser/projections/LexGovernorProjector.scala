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

  def apply(anch: LexGrammar[L,W]#Spec, chart: ParseMarginal[L, W]):LexGovernorInfo = {
    val v = new Visitor(anch, chart.length)
    chart.visit(v)
    LexGovernorInfo(v.spanType, v.spanGovernorCounts, v.wordGovernorCounts, v.maximalLabelType)
  }

  // WHENEVER you change this, be sure to change PropertyParsingAnchoring
  private class Visitor(spec: LexGrammar[L, W]#Spec, length: Int) extends AnchoredVisitor[L] {
    val spanType = TriangularArray.fill(length)(labelBeliefs)
    val spanGovernorCounts = TriangularArray.fill(length)(baseDV)
    val wordGovernorCounts = Array.fill(length)(DenseVector.zeros[Double](length+1))
    val maximalLabelType = Array.fill(length)(labelBeliefs)
    val wordTagType = Array.fill(length)(labelBeliefs)

    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val head = spec.headIndex(ref)
      val dep = spec.depIndex(ref)
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
      val head = spec.unaryHeadIndex(ref)
      if (begin == 0 && end == length) { // root, get the length
        wordGovernorCounts(head)(length) += score
        spanGovernorCounts(begin, end)(length) += score
        spanGovernorCounts(begin, end)(notASpan) -= score
        maximalLabelType(head)(tag) += score
        maximalLabelType(head)(tag) -= score
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
