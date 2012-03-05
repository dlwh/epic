package scalanlp.parser.lex

import scalanlp.tensor.sparse.OldSparseVector
import scalanlp.util.{Encoder, Index}
import scalanlp.parser.{UnaryRule, BinaryRule, Rule}

/**
 * 
 * @author dlwh
 */
trait LexGrammar[L,W] {

  def index: Index[Rule[L]]
  def labelIndex: Index[L]

  def labelEncoder = Encoder.fromIndex(labelIndex)

  val allLabels = Array.range(0,labelIndex.size)
  def labelsForHead(head: W):Array[Int] = allLabels
  def rulesForLabel(label: Int):Array[Int]
//  def rulesForLabelHead(label: Int, head: W):Array[Int]

  def scoreLeftComplement(rule: Int, words: Seq[W], head: Int, leftHead: Int):Double
  def scoreRightComplement(rule: Int, words: Seq[W], head: Int, rightHead: Int):Double
  def scoreUnary(rule: Int, words: Seq[W], head: Int):Double

  def tagScores(words: Seq[W], head: Int):OldSparseVector

  def maxNumBinaryRulesForParent:Int

  // don't mutate me!
  val indexedRules: Array[Rule[Int]];

  def ruleIndex(a: Int, b: Int, c: Int):Int
  def ruleIndex(a: Int, b: Int):Int

  // Rule Index
  def indexedBinaryRulesWithParent(l: Int):Array[Int]
  // Rule Index
  def indexedBinaryRulesWithLeftChild(b: Int):Array[Int]
  // Rule Index
  def indexedBinaryRulesWithRightChild(c: Int):Array[Int]
  // Rule Index
  def indexedUnaryRulesWithChild(l: Int):Array[Int]
  // Rule Index
  def indexedUnaryRulesWithParent(l: Int):Array[Int]

  def parent(r: Int) = indexedRules(r).parent
  def leftChild(r: Int) = indexedRules(r).asInstanceOf[BinaryRule[Int]].left
  def rightChild(r: Int) = indexedRules(r).asInstanceOf[BinaryRule[Int]].right
  def child(r: Int) = indexedRules(r).asInstanceOf[UnaryRule[Int]].child
}

