package epic.parser
package projections

import breeze.collection.mutable.TriangularArray
import breeze.linalg._
import models.LexGrammar


/**
 * 
 * @author dlwh
 */
class LexGovernorProjector[L, W](grammar: LexGrammar[L, W]) {
  val notAConstituent = grammar.labelIndex.size

  def apply(anch: RefinedAnchoring[L, W], chart: ParseMarginal[L, W]):LexGovernorInfo = {
    assert(anch.annotationTag == 1)
    val v = new Visitor(anch, chart.length)
    chart.visit(v)
    LexGovernorInfo(v.spanType, v.spanGovernorCounts, v.governedSpan, v.wordGovernorCounts, v.wordTagType, v.maximalLabelType)
  }

  // WHENEVER you change this, be sure to change PropertyParsingAnchoring
  private class Visitor(spec: RefinedAnchoring[L, W], length: Int) extends AnchoredVisitor[L] {
    val spanType = TriangularArray.fill(length+1)(optionalLabelBeliefs)
    val spanGovernorCounts = TriangularArray.fill(length+1)(baseDV)
    val wordGovernorCounts = Array.fill(length)(DenseVector.zeros[Double](length+1))
    val governedSpan = Array.fill(length)(DenseVector.zeros[Double](TriangularArray.arraySize(length+1)))
    val maximalLabelType = Array.fill(length)(labelBeliefs)
    val wordTagType = Array.fill(length)(labelBeliefs)
    val otherAnch = grammar.anchor(spec.words)

    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val head = otherAnch.headIndex(ref)
      val dep = otherAnch.depIndex(ref)
      wordGovernorCounts(dep)(head) += score
      if (grammar.isRightRule(rule)) { // head on the right
        spanGovernorCounts(begin, split)(head) += score
        spanGovernorCounts(begin, split)(notASpan) -= score
        val label = grammar.grammar.leftChild(rule)
        maximalLabelType(dep)(label) += score
        governedSpan(dep)(TriangularArray.index(begin,split)) += score
      } else {
        spanGovernorCounts(split, end)(head) += score
        spanGovernorCounts(split, end)(notASpan) -= score
        val label = grammar.grammar.rightChild(rule)
        maximalLabelType(dep)(label) += score
        governedSpan(dep)(TriangularArray.index(split,end)) += score
      }
    }

    def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val parent = grammar.grammar.parent(rule)
      spanType(begin,end)(parent) += score
      spanType(begin,end)(notAConstituent) -= score
      val head = otherAnch.spanHeadIndex(ref)

      if (begin == 0 && end == length) { // root, get the length
        wordGovernorCounts(head)(length) += score
        spanGovernorCounts(begin, end)(length) += score
        spanGovernorCounts(begin, end)(notASpan) -= score
        maximalLabelType(head)(parent) += score
        governedSpan(head)(TriangularArray.index(begin,end)) += score
      }
    }

    def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {


      if(begin + 1 == end) {
        wordTagType(begin)(tag) += score
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
      val r = grammar.grammar.labelEncoder.mkDenseVector()
      r
    }

    def optionalLabelBeliefs = {
      val r = DenseVector.zeros[Double](grammar.grammar.labelIndex.size + 1)
      r(notAConstituent) = 1.0
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
                           governedSpan: Array[DenseVector[Double]],
                           wordGovernor: Array[DenseVector[Double]],
                           wordTag: Array[DenseVector[Double]],
                           maximalLabelType: Array[DenseVector[Double]])
