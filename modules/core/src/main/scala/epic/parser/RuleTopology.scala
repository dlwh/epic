package epic.parser
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
import collection.mutable.ArrayBuffer
import breeze.util.{Index, Encoder}
import breeze.linalg.Counter2
import epic.trees._
import java.io.ObjectStreamException


/**
 * A RuleTopology is basically a grammar with no weights. It provides
 * lookup methods by rule, by parent, etc.
 *
 * @author dlwh
 */
@SerialVersionUID(2L)
final class RuleTopology[L] private (
                                   /** The "start" symbol. Usually "TOP" in this parser. */
                                     val root: L,
                                    /** Index for all symbols, including synthetic symbols from binarization */
                                    val labelIndex: Index[L],
                                    /** Index over all acceptable rules */
                                    val index: Index[Rule[L]],
                                    val indexedRules: Array[Rule[Int]],
                                    binaryRulesByParent: Array[Array[Int]],
                                    unaryRulesByParent: Array[Array[Int]],
                                    binaryRulesByLeftChild: Array[Array[Int]],
                                    binaryRulesByRightChild: Array[Array[Int]],
                                    unaryRulesByChild: Array[Array[Int]]) extends Encoder[Rule[L]] with Serializable {

  val rootIndex = labelIndex(root)

  def labelEncoder  = Encoder.fromIndex(labelIndex)

  // Accessors for properties of indexed rules
  /** Returns the parent label index from the rule index */
  def parent(r: Int): Int = indexedRules(r).parent
  /** Returns the left child label index from the rule index */
  def leftChild(r: Int): Int = indexedRules(r).asInstanceOf[BinaryRule[Int]].left
  /** Returns the right child label index from the rule index */
  def rightChild(r: Int): Int = indexedRules(r).asInstanceOf[BinaryRule[Int]].right
  /** Returns the child label index from the (unary) rule index */
  def child(r: Int): Int = indexedRules(r).asInstanceOf[UnaryRule[Int]].child
  def chain(r: Int): IndexedSeq[String] = indexedRules(r).asInstanceOf[UnaryRule[Int]].chain
  def isBinary(r: Int) = indexedRules(r).isInstanceOf[BinaryRule[_]]

  def indexedRule(r: Int) = indexedRules(r)

  // query by parent or child
  /** Gives all binary rule indices with this parent */
  def indexedBinaryRulesWithParent(l: Int) = binaryRulesByParent(l)
  /** Gives all unary rule indices with this parent */
  def indexedUnaryRulesWithParent(l: Int) = unaryRulesByParent(l)
  /** Gives all unary rule indices with this child */
  def indexedUnaryRulesWithChild(l: Int) = unaryRulesByChild(l)

  /** Gives all binary rule indices with this left child */
  def indexedBinaryRulesWithLeftChild(b: Int) = binaryRulesByLeftChild(b)

  /** Gives all binary rule indices with this right child */
  def indexedBinaryRulesWithRightChild(c: Int) = binaryRulesByRightChild(c)

  def prettyString = {
    val builder = new StringBuilder()
    builder ++= ("Root: " + root.toString + "\n")
//    builder ++= labelIndex.addString(builder, "Labels:\n", ", ", "\n\n")
    val labelStrings = labelIndex.map(_.toString).toIndexedSeq
    val startLength = labelStrings.view.map(_.length).max + 1
    val blocks = indexedRules.groupBy(_.parent)
    for( (parent,block) <- blocks) {
      var first = true
      for (r <- block) {
        if (!first)
          builder ++= (" "*startLength)
        else
          builder ++= labelStrings(parent).padTo(startLength, ' ')

        builder ++= "-> "

        r match {
          case UnaryRule(a, b, chain) =>
            if (chain.nonEmpty)
              chain.addString(builder, "(", "^", ")^")
            builder ++= labelStrings(b)
          case BinaryRule(a, b, c) =>
            builder ++= labelStrings(b)
            builder += ' '
            builder ++= labelStrings(c)
        }
        builder += '\n'

        first = false

      }
    }
    builder.toString()
  }

  @throws(classOf[ObjectStreamException])
  private def writeReplace():Object  = {
    new RuleTopology.SerializedForm(root, labelIndex, index)
  }
}

object RuleTopology {
  /** Builds a grammar just from some productions */
  def apply[L, W](root: L, productions: TraversableOnce[Rule[L]]): RuleTopology[L] = {
    val index = Index[L]()
    val ruleIndex = Index[Rule[L]]()
    for(r <- productions) {
      index.index(r.parent)
      r.children.foreach(index.index(_))
      ruleIndex.index(r)
    }
    apply(root, index, ruleIndex)
  }

  /**
   * Given a bunch of counts, builds a grammar.
   * @param root the root label
   * @param binaries presumably counts of binary rules
   * @param unaries presumably counts of unary rules
   * @tparam L label type
   * @return a base grammar instance
   */
  def apply[L](root: L,
               binaries: Counter2[L, _<:Rule[L], _],
               unaries: Counter2[L, _ <: Rule[L], _]): RuleTopology[L] = {
    apply(root, binaries.keysIterator.map(_._2) ++ unaries.keysIterator.map(_._2))
  }

  /**
   * Given the indices necessary to make a grammar, builds the other data structures
   * that enable fast access to parent/child information, etc.
   * @param root root label
   * @param labelIndex index of grammar symbols
   * @param ruleIndex index of rules
   * @return
   */
  def apply[L, W](root: L,
                  labelIndex: Index[L],
                  ruleIndex: Index[Rule[L]]):RuleTopology[L] = {
    val indexedRules = for ( r <- ruleIndex.toArray) yield r match {
      case BinaryRule(a, b, c) => BinaryRule(labelIndex(a), labelIndex(b), labelIndex(c)):Rule[Int]
      case UnaryRule(a, b, chain) => UnaryRule(labelIndex(a), labelIndex(b), chain):Rule[Int]
    }

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
      case UnaryRule(p, c, chain) =>
        unaryRulesByParent(p) += i
        unaryRulesByChild(c) += i
    }

    new RuleTopology(
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

  @SerialVersionUID(1)
  private class SerializedForm[L](var root: L, var labelIndex: Index[L], var ri: Index[Rule[L]]) extends Serializable {
    @throws(classOf[ObjectStreamException])
    protected def readResolve():Object = {
      apply(root, labelIndex, ri)
    }
  }

}



