package epic.parser

import epic.trees.{UnaryRule, BinaryRule, Span}
import scala.collection.mutable.ArrayBuffer
import breeze.linalg.CSCMatrix

/**
 * This class is mostly an implementation detail. It's used for
 * [[epic.parser.SimpleGrammar]] instances to accelerate parsing. It's kind of
 * like a compressed sparse columns matrix ([[breeze.linalg.CSCMatrix]]), but it's a
 * 3-d tensor  instead of a 2-d matrix.
 *
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
final class SparseRuleTensor[L] private(val leftChildOffsets: Array[Int],
                               val rightChildIndicesAndOffsets: Array[Int],
                               val parentIndicesAndScores: Array[Int],
                               val unaryChildPtrs: Array[Int],
                               val unaryParentIndicesAndScores: Array[Int],
                               outside: Boolean) extends Serializable {

  val numLeftChildren = leftChildOffsets.length - 1

  def leftChildRange(lc: Int):Span = Span(leftChildOffsets(lc), leftChildOffsets(lc+1) )

  def rightChildForOffset(rcOff: Int) = rightChildIndicesAndOffsets(rcOff * 2)

  def rightChildRange(rcOff: Int):Span = Span(rightChildIndicesAndOffsets(rcOff * 2 + 1), rightChildIndicesAndOffsets(rcOff * 2 + 3))

  def parentForOffset(off: Int) = parentIndicesAndScores(off * 3)

  def ruleScoreForOffset(off: Int): Double = {
    val first = parentIndicesAndScores(off * 3 + 1)
    val second = parentIndicesAndScores(off * 3 + 2)
    java.lang.Double.longBitsToDouble((first.toLong << 32) | (second.toLong&0xFFFFFFFFL))
  }

  def unaryChildRange(lc: Int):Span = Span(unaryChildPtrs(lc), unaryChildPtrs(lc+1) )

  def unaryParentForOffset(off: Int) = unaryParentIndicesAndScores(off * 3)

  def unaryScoreForOffset(off: Int): Double = {
    val first = unaryParentIndicesAndScores(off * 3 + 1)
    val second = unaryParentIndicesAndScores(off * 3 + 2)
    java.lang.Double.longBitsToDouble((first.toLong << 32) | (second.toLong&0xFFFFFFFFL))
  }

  def ruleIterator:Iterator[(BinaryRule[Int], Double)] = {
    for {
      lc <- Iterator.range(0, leftChildOffsets.length - 1)
      rcOff <- leftChildRange(lc).iterator
      rc = rightChildForOffset(rcOff)
      pOff <- rightChildRange(rcOff).iterator
      p = parentForOffset(pOff)
      score = ruleScoreForOffset(pOff)
    } yield {
      if (outside)
        BinaryRule(lc, rc, p) -> score
      else
        BinaryRule(p, lc, rc) -> score
    }
  }

}

object SparseRuleTensor {
  def forSimpleGrammarInside[L, L2, W](grammar: SimpleGrammar[L, L2, W]):SparseRuleTensor[L2] = {
    import grammar.refinedTopology._
    val orderedRuleIndices = (0 until index.size).filter(index.get(_).isInstanceOf[BinaryRule[_]]).sorted(BinaryRule.leftChildFirstOrdering[Int].on(indexedRules(_:Int).asInstanceOf[BinaryRule[Int]]))
    val leftChildOffsets            = new ArrayBuffer[Int]
    val rightChildIndicesAndOffsets = new ArrayBuffer[Int]
    val parentIndicesAndScores      = new ArrayBuffer[Int]()

    var lastLc     = -1
    var lastRc     = -1
    var lastRcOffset = 0
    var lastOffset = 0

    // leftChildOffsets += 0

    for(r <- orderedRuleIndices) {
      val lc = leftChild(r)
      var endRightChild = false
      assert(lastLc <= lc)
      while (lastLc != lc) {
        lastLc += 1
        leftChildOffsets += lastRcOffset
        endRightChild = true
      }

      val rc = rightChild(r)
      if (endRightChild || rc != lastRc) {
        rightChildIndicesAndOffsets += rc
        rightChildIndicesAndOffsets += lastOffset
        lastRc = rc
        lastRcOffset += 1
      }

      val p = parent(r)
      val rs = grammar.ruleScore(r)
      val span: Span = new Span(java.lang.Double.doubleToLongBits(rs))
      val encodedFirst = span.begin
      val encodedSecond = span.end
      parentIndicesAndScores += (p, encodedFirst, encodedSecond)

      lastOffset += 1
      assert(parentIndicesAndScores.length == lastOffset * 3)
    }
    leftChildOffsets += lastRcOffset
    rightChildIndicesAndOffsets += -1
    rightChildIndicesAndOffsets += lastOffset

    val unaryChildOffsets            = new ArrayBuffer[Int]
    val unaryParentIndicesAndScores = new ArrayBuffer[Int]

    val unaryRules = (0 until index.size).filter(index.get(_).isInstanceOf[UnaryRule[_]]).sorted(UnaryRule.childFirstOrdering[Int].on(indexedRules(_:Int).asInstanceOf[UnaryRule[Int]]))
    lastLc = -1
    lastOffset = 0
    for(r <- unaryRules) {
      val lc = child(r)
      assert(lastLc <= lc)
      while (lastLc != lc) {
        unaryChildOffsets += lastOffset
        lastLc += 1
      }

      val p = parent(r)
      val rs = grammar.ruleScore(r)
      val span: Span = new Span(java.lang.Double.doubleToLongBits(rs))
      val encodedFirst = span.begin
      val encodedSecond = span.end
      unaryParentIndicesAndScores += (p, encodedFirst, encodedSecond)
      lastOffset += 1
    }
    while (lastLc <= grammar.refinedTopology.labelIndex.size) {
      lastLc += 1
      unaryChildOffsets += lastOffset
    }

    val ret = new SparseRuleTensor[L2](leftChildOffsets.toArray, rightChildIndicesAndOffsets.toArray, parentIndicesAndScores.toArray, unaryChildOffsets.toArray, unaryParentIndicesAndScores.toArray, false)

    assert(ret.ruleIterator.map(_._1).toIndexedSeq == orderedRuleIndices.map(indexedRules(_)), s"\n${ret.ruleIterator.map(_._1).toIndexedSeq}\n${orderedRuleIndices.map(indexedRules(_))}")
    assert(ret.ruleIterator.map(_._2).toIndexedSeq == orderedRuleIndices.map(grammar.ruleScore(_)))

    ret
  }

  def forSimpleGrammarOutside[L, L2, W](grammar: SimpleGrammar[L, L2, W]):SparseRuleTensor[L2] = {
    import grammar.refinedTopology._
    val orderedRuleIndices = (0 until index.size).filter(index.get(_).isInstanceOf[BinaryRule[_]]).sorted(BinaryRule.parentFirstOrdering[Int].on(indexedRules(_:Int).asInstanceOf[BinaryRule[Int]]))
    val leftChildOffsets            = new ArrayBuffer[Int]
    val rightChildIndicesAndOffsets = new ArrayBuffer[Int]
    val parentIndicesAndScores      = new ArrayBuffer[Int]()

    var lastLc     = -1
    var lastRc     = -1
    var lastRcOffset = 0
    var lastOffset = 0

    // leftChildOffsets += 0

    for(r <- orderedRuleIndices) {
      val lc = parent(r)
      var endRightChild = false
      assert(lastLc <= lc)
      while (lastLc != lc) {
        lastLc += 1
        leftChildOffsets += lastRcOffset
        endRightChild = true
      }

      val rc = leftChild(r)
      if (endRightChild || rc != lastRc) {
        rightChildIndicesAndOffsets += rc
        rightChildIndicesAndOffsets += lastOffset
        lastRc = rc
        lastRcOffset += 1
      }

      val p = rightChild(r)
      val rs = grammar.ruleScore(r)
      val span: Span = new Span(java.lang.Double.doubleToLongBits(rs))
      val encodedFirst = span.begin
      val encodedSecond = span.end
      parentIndicesAndScores += (p, encodedFirst, encodedSecond)

      lastOffset += 1
      assert(parentIndicesAndScores.length == lastOffset * 3)
    }
    leftChildOffsets += lastRcOffset
    rightChildIndicesAndOffsets += -1
    rightChildIndicesAndOffsets += lastOffset

    val unaryChildOffsets            = new ArrayBuffer[Int]
    val unaryParentIndicesAndScores = new ArrayBuffer[Int]

    val unaryRules = (0 until index.size).filter(index.get(_).isInstanceOf[UnaryRule[_]]).sorted(UnaryRule.parentFirstOrdering[Int].on(indexedRules(_:Int).asInstanceOf[UnaryRule[Int]]))
    lastLc = -1
    lastOffset = 0
    for(r <- unaryRules) {
      val lc = parent(r)
      while (lastLc != lc) {
        unaryChildOffsets += lastOffset
        lastLc += 1
      }

      val p = child(r)
      val rs = grammar.ruleScore(r)
      val span: Span = new Span(java.lang.Double.doubleToLongBits(rs))
      val encodedFirst = span.begin
      val encodedSecond = span.end
      unaryParentIndicesAndScores += (p, encodedFirst, encodedSecond)
      lastOffset += 1
    }
    while (lastLc <= labelIndex.size) {
      lastLc += 1
      unaryChildOffsets += lastOffset
    }

    val ret = new SparseRuleTensor[L2](leftChildOffsets.toArray, rightChildIndicesAndOffsets.toArray, parentIndicesAndScores.toArray, unaryChildOffsets.toArray, unaryParentIndicesAndScores.toArray, true)

    assert(ret.ruleIterator.map(_._1).toIndexedSeq == orderedRuleIndices.map(indexedRules(_)), s"\n${ret.ruleIterator.map(_._1).toIndexedSeq}\n${orderedRuleIndices.map(indexedRules(_))}")
    assert(ret.ruleIterator.map(_._2).toIndexedSeq == orderedRuleIndices.map(grammar.ruleScore(_)))

    ret
  }

}