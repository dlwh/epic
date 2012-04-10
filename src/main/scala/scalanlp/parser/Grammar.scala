package scalanlp.parser

import scalanlp.collection.mutable.OpenAddressHashArray
import collection.mutable.ArrayBuffer
import scalanlp.trees.{UnaryRule, Rule, BinaryRule}
import scalanlp.util.{TypeTags, Index, Encoder}
import TypeTags.{ID,tag,tagArray}
import scalala.tensor.Counter2


/**
 * A minimalist grammar that encodes just enough information to get reachability.
 *
 * That is, it has enough information to decide what parse trees for a given sentence are admissible
 * given a tagged sentence.
 *
 *
 * @author dlwh
 */
trait Grammar[L] extends Encoder[Rule[L]] with Serializable {
  override val index: Index[Rule[L]]
  val labelIndex: Index[L];
  def labelEncoder  = Encoder.fromIndex(labelIndex)
  protected val indexedRules: Array[Rule[Int]];

  def root: L

  // Accessors for properties of indexed rules
  def parent(r: Int):Int = tag[L](indexedRules(r).parent)
  def leftChild(r: Int): Int = TypeTags.tag[L](indexedRules(r).asInstanceOf[BinaryRule[Int]].left)
  def rightChild(r: Int): Int = tag[L](indexedRules(r).asInstanceOf[BinaryRule[Int]].right)
  def child(r: Int): Int = tag[L](indexedRules(r).asInstanceOf[UnaryRule[Int]].child)

  // query by parent or child
  def indexedBinaryRulesWithParent(l: Int):Array[Int]
  def indexedBinaryRulesWithLeftChild(b: Int):Array[Int]
  def indexedBinaryRulesWithRightChild(c: Int):Array[Int]
  def indexedUnaryRulesWithChild(l: Int):Array[Int]
  def indexedUnaryRulesWithParent(l: Int):Array[Int]

  val tags: Set[L]
  val indexedTags: Array[Int]
}

object Grammar {
  def apply[L](root: L, productions: TraversableOnce[Rule[L]], tags: TraversableOnce[L]): Grammar[L] = {
    val index = Index[L]();
    val ruleIndex = Index[Rule[L]]()
    for(r <- productions) {
      index.index(r.parent);
      r.children.foreach(index.index(_))
      ruleIndex.index(r)
    }
    apply(root: L, index, ruleIndex, tags)
  }
  
  def apply[L](root: L, binaries: Counter2[L, _<:Rule[L], _], unaries: Counter2[L, _ <: Rule[L], _], tags: Counter2[L, _, Double]): Grammar[L] = {
    apply(root, binaries.keysIterator.map(_._2) ++ unaries.keysIterator.map(_._2), tags.keysIterator.map(_._1))
  }

  def apply[L, W](root: L,
                  labelIndex: Index[L],
                  ruleIndex: Index[Rule[L]],
                  tags: TraversableOnce[L]):Grammar[L] = {
    val li = labelIndex
    val ri = ruleIndex
    val r = root
    val t = tags.toSet

    new Grammar[L] {
      val index = ri
      val labelIndex = li
      def root = r

      val indexedRules = for ( r <- index.toArray) yield r match {
        case BinaryRule(a, b, c) => BinaryRule(labelIndex(a), labelIndex(b), labelIndex(c)):Rule[Int]
        case UnaryRule(a, b) => UnaryRule(labelIndex(a), labelIndex(b)):Rule[Int]
      }

      val binaryRuleTable = labelEncoder.fillArray(new OpenAddressHashArray[Int](labelIndex.size * labelIndex.size, -1, 4))
      val unaryRuleTable = new OpenAddressHashArray[Int](labelIndex.size * labelIndex.size, -1)

      private val (binaryRulesByParent, unaryRulesByParent, binaryRulesByLeftChild, binaryRulesByRightChild, unaryRulesByChild) = {
        val binaryRulesByParent = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
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

        (binaryRulesByParent.map(_.toArray),
          unaryRulesByParent.map(_.toArray),
          binaryRulesByLeftChild.map(_.toArray),
          binaryRulesByRightChild.map(_.toArray),
          unaryRulesByChild.map(_.toArray))
      }

      val tags = t

      val indexedTags = (tags.map(labelIndex).toArray)

      def ruleIndex(a: Int, b: Int, c: Int) = binaryRuleTable(a)(c + labelIndex.size * b)
      def ruleIndex(a: Int, b: Int) = unaryRuleTable(b + labelIndex.size * a)

      def indexedBinaryRulesWithParent(l: Int) = binaryRulesByParent(l)
      def indexedUnaryRulesWithParent(l: Int) = unaryRulesByParent(l)
      def indexedUnaryRulesWithChild(l: Int) = unaryRulesByChild(l)
      def indexedBinaryRulesWithLeftChild(b: Int) = binaryRulesByLeftChild(b)
      def indexedBinaryRulesWithRightChild(c: Int) = binaryRulesByRightChild(c)

    }

  }
}


