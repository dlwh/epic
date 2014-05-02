package epic.parser
package projections

import breeze.collection.mutable.TriangularArray
import breeze.linalg._
import models.LexGrammar


/**
 * This class is used to get information about the distribution of dependencies and label types
 * in a lexicalized chart.
 * @author dlwh
 */
class LexGovernorProjector[L, W](grammar: LexGrammar[L, _, W]) {
  val notAConstituent = grammar.labelIndex.size

  def apply(anch: RefinedAnchoring[L, W], chart: ParseMarginal[L, W]):LexGovernorInfo = {
    assert(anch.annotationTag == 1)
    val v = new Visitor(anch, chart.length)
    chart.visit(v)
    import v._
    LexGovernorInfo(v.spanType, v.spanHeadCounts, v.governedSpan, v.wordGovernorCounts, v.wordTagType, v.maximalLabelType)
  }

  /** WHENEVER you change this, be sure to change [[epic.everything.SentLexParser.Anchoring]] */
  private class Visitor(spec: RefinedAnchoring[L, W], length: Int) extends AnchoredVisitor[L] {
    val spanType = TriangularArray.fill(length+1)(optionalLabelBeliefs)
    // which word is the head of which span
    val spanHeadCounts = TriangularArray.fill(length+1)(optionalGovernorCounts)
    val wordGovernorCounts = Array.fill(length)(DenseVector.zeros[Double](length+1))
    val governedSpan = Array.fill(length)(DenseVector.zeros[Double](TriangularArray.arraySize(length+1)))
    val maximalLabelType = Array.fill(length)(labelBeliefs)
    val wordTagType = Array.fill(length)(labelBeliefs)
    val otherAnch = grammar.anchor(spec.words)

    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val head = otherAnch.headIndex(ref)
      val dep = otherAnch.depIndex(ref)
      wordGovernorCounts(dep)(head) += score
      siphonMass(spanHeadCounts(begin, split), head, notASpan, score)
      siphonMass(spanHeadCounts(split, end), head, notASpan, score)
      if (grammar.isHeadOnRightForRule(rule)) { // head on the right
        assert(head >= split)
        val label = grammar.topology.leftChild(rule)
        maximalLabelType(dep)(label) += score
        governedSpan(dep)(TriangularArray.index(begin,split)) += score
      } else {
        assert(head < split)
        val label = grammar.topology.rightChild(rule)
        maximalLabelType(dep)(label) += score
        governedSpan(dep)(TriangularArray.index(split,end)) += score
      }
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

    def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val parent = grammar.topology.parent(rule)
      siphonMass(spanType(begin,end), parent, notAConstituent, score)
      val head = otherAnch.unaryHeadIndex(ref)

      if (begin == 0 && end == length) { // root, get the length
        wordGovernorCounts(head)(length) += score
        siphonMass(spanHeadCounts(begin,end), length, notASpan, score)
        maximalLabelType(head)(parent) += score
        governedSpan(head)(TriangularArray.index(begin,end)) += score
      }
    }

    def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {

      if(begin + 1 == end) {
        wordTagType(begin)(tag) += score
      }
    }

    def optionalGovernorCounts = {
      val r = DenseVector.zeros[Double](length+2)
      // length is root, length + 1
      // start with all mass in "is off", and subtract
      r(notASpan) = 1.0
      r
    }

    private def notASpan = length+1
    def labelBeliefs = {
      val r = grammar.topology.labelEncoder.mkDenseVector()
      r
    }

    def optionalLabelBeliefs = {
      val r = DenseVector.zeros[Double](grammar.topology.labelIndex.size + 1)
      r(notAConstituent) = 1.0
      r
    }
  }
}

/**
 * Holds information produced by LexGovernorProjector. Basically a hodgepodge
 * of marginal statistics about dependencies and label types.
 * @param spanType the label of the span. labelIndex.size + 1 is notASpan
 * @param spanGovernor which word governs my span. not my head. my head's head. length = root (i.e. whole setnence), length+1 == off
 * @param wordGovernor which word governs me, i.e. head. length = root
 * @param wordTag the pos tag
 * @param maximalLabelType for each word the type of its maximal projection
 */
case class LexGovernorInfo(spanType: TriangularArray[DenseVector[Double]],
                           spanGovernor: TriangularArray[DenseVector[Double]],
                           governedSpan: Array[DenseVector[Double]],
                           wordGovernor: Array[DenseVector[Double]],
                           wordTag: Array[DenseVector[Double]],
                           maximalLabelType: Array[DenseVector[Double]])
