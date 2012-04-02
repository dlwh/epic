package scalanlp.parser.lex

import scalanlp.tensor.sparse.OldSparseVector
import scalanlp.util.{Encoder, Index}
import scalanlp.trees.{UnaryRule, BinaryRule, Rule}
import collection.immutable.BitSet

/**
 * 
 * @author dlwh
 */
trait LexGrammar[L,W] extends Serializable {

  def index: Index[Rule[L]]
  def labelIndex: Index[L]
  def wordIndex: Index[W]

  def labelEncoder = Encoder.fromIndex(labelIndex)
  def rulesForLabel(label: Int):Array[Int] = indexedBinaryRulesWithParent(label)

  def specialize(sent: Seq[W]):Specialization

  trait Specialization {
    def words: Seq[W]
    def labelsForHead(headPos: Int):IndexedSeq[Int] = 0 until labelIndex.size
    def scoreLeftComplement(rule: Int, head: Int, leftHead: Int):Double
    def scoreRightComplement(rule: Int, head: Int, rightHead: Int):Double
    def scoreUnary(rule: Int, head: Int):Double
    def tagScores(pos: Int):OldSparseVector
  }

//  def rulesForLabelHead(label: Int, head: W):Array[Int]


  def tags: IndexedSeq[L]
  def indexedTags: BitSet

  def maxNumBinaryRulesForParent:Int

  // Left Rule if the left child is the head
  def isLeftRule(r: Int):Boolean
  // Right rule if the right child is the head
  def isRightRule(r: Int):Boolean

  // don't mutate me!
  val indexedRules: Array[Rule[Int]];

  def ruleIndex(a: Int, b: Int, c: Int):Int
  def ruleIndex(a: Int, b: Int):Int

  // Rule Index
  def indexedBinaryRulesWithParent(l: Int):Array[Int]
  // Rule Index
  def indexedUnaryRulesWithChild(l: Int):Array[Int]
  // Rule Index
  def indexedUnaryRulesWithParent(l: Int):Array[Int]

  def parent(r: Int) = indexedRules(r).parent
  def leftChild(r: Int) = indexedRules(r).asInstanceOf[BinaryRule[Int]].left
  def rightChild(r: Int) = indexedRules(r).asInstanceOf[BinaryRule[Int]].right
  def child(r: Int) = indexedRules(r).asInstanceOf[UnaryRule[Int]].child
}

