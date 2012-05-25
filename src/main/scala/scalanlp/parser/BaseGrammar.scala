package scalanlp.parser

import scalanlp.collection.mutable.OpenAddressHashArray
import collection.mutable.ArrayBuffer
import scalanlp.util.{Index, Encoder}
import scalala.tensor.Counter2
import scalanlp.trees._


/**
 * A minimalist grammar that encodes just enough information to get reachability.
 *
 * That is, it has enough information to decide what parse trees for a given sentence are admissible
 * given a tagged sentence.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
final class BaseGrammar[L] private (val root: L,
                                    val labelIndex: Index[L],
                                    val index: Index[Rule[L]],
                                    indexedRules: Array[Rule[Int]],
                                    binaryRulesByParent: Array[Array[Int]],
                                    unaryRulesByParent: Array[Array[Int]],
                                    binaryRulesByLeftChild: Array[Array[Int]],
                                    binaryRulesByRightChild: Array[Array[Int]],
                                    unaryRulesByChild: Array[Array[Int]]) extends Encoder[Rule[L]] with Serializable {
  def labelEncoder  = Encoder.fromIndex(labelIndex)

  // Accessors for properties of indexed rules
  def parent(r: Int):Int = indexedRules(r).parent
  def leftChild(r: Int): Int = indexedRules(r).asInstanceOf[BinaryRule[Int]].left
  def rightChild(r: Int): Int = indexedRules(r).asInstanceOf[BinaryRule[Int]].right
  def child(r: Int): Int = indexedRules(r).asInstanceOf[UnaryRule[Int]].child

  // query by parent or child
  def indexedBinaryRulesWithParent(l: Int) = binaryRulesByParent(l)
  def indexedUnaryRulesWithParent(l: Int) = unaryRulesByParent(l)
  def indexedUnaryRulesWithChild(l: Int) = unaryRulesByChild(l)
  def indexedBinaryRulesWithLeftChild(b: Int) = binaryRulesByLeftChild(b)
  def indexedBinaryRulesWithRightChild(c: Int) = binaryRulesByRightChild(c)
}

object BaseGrammar {
  def apply[L, W](root: L, productions: TraversableOnce[Rule[L]]): BaseGrammar[L] = {
    val index = Index[L]();
    val ruleIndex = Index[Rule[L]]()
    val lex = new ArrayBuffer[LexicalProduction[L, W]]()
    for(r <- productions) {
      index.index(r.parent);
      r.children.foreach(index.index(_))
      ruleIndex.index(r)
    }
    apply(root, index, ruleIndex)
  }
  
  def apply[L](root: L,
               binaries: Counter2[L, _<:Rule[L], _],
               unaries: Counter2[L, _ <: Rule[L], _]): BaseGrammar[L] = {
    apply(root, binaries.keysIterator.map(_._2) ++ unaries.keysIterator.map(_._2))
  }

  def apply[L, W](root: L,
                  labelIndex: Index[L],
                  ruleIndex: Index[Rule[L]]):BaseGrammar[L] = {
    val indexedRules = for ( r <- ruleIndex.toArray) yield r match {
      case BinaryRule(a, b, c) => BinaryRule(labelIndex(a), labelIndex(b), labelIndex(c)):Rule[Int]
      case UnaryRule(a, b) => UnaryRule(labelIndex(a), labelIndex(b)):Rule[Int]
    }

    val binaryRuleTable = Array.fill(labelIndex.size)(new OpenAddressHashArray[Int](labelIndex.size * labelIndex.size, -1, 4))
    val unaryRuleTable = new OpenAddressHashArray[Int](labelIndex.size * labelIndex.size, -1)

    val binaryRulesByParent: Array[ArrayBuffer[Int]] = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
    val unaryRulesByParent = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
    val binaryRulesByLeftChild = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
    val binaryRulesByRightChild = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
    val unaryRulesByChild = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
    for ( (r, i) <- indexedRules.zipWithIndex) r match {
      case BinaryRule(p, l, rc) =>
        binaryRulesByParent(p) += i
        binaryRulesByLeftChild(l) += i
        binaryRulesByRightChild(rc) += i
        binaryRuleTable(p)(rc + labelIndex.size * l) = i
      case UnaryRule(p, c) =>
        unaryRulesByParent(p) += i
        unaryRulesByChild(c) += i
        unaryRuleTable(c + labelIndex.size * (p)) = i
    }

    new BaseGrammar(
      root,
      labelIndex,
      ruleIndex,
      indexedRules,
      binaryRulesByParent.map(_.toArray),
      unaryRulesByParent.map(_.toArray),
      binaryRulesByLeftChild.map(_.toArray),
      binaryRulesByRightChild.map(_.toArray),
      unaryRulesByChild.map(_.toArray))
  }
}


