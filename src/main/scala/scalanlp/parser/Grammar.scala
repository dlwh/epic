package scalanlp.parser

import scalala.Scalala._;
import scalala.tensor._;
import scalanlp.data.VectorBroker
import scalanlp.collection.mutable.SparseArray;
import scalanlp.counters.LogCounters._;
import scalanlp.util.Index;

sealed abstract class Rule[+L] { def parent: L }
final case class BinaryRule[+L](parent: L, left: L, right: L) extends Rule[L];
final case class UnaryRule[+L](parent: L, child: L) extends Rule[L];

trait Grammar[L] extends VectorBroker[L] {
  def unaryRulesByChild(c: L): Iterator[(UnaryRule[L],Double)];
  def unaryRulesByParent(p: L): Iterator[(UnaryRule[L],Double)];
  def binaryRulesByLeftChild(c: L): Iterator[(BinaryRule[L],Double)];
  def unaryRulesByIndexedChild(c: Int): Vector;
  def binaryRulesByIndexedLeftChild(b: Int): SparseArray[Vector];
}

/**
 * Given a counter of productions that has been log-normalized by rows,
 */
class GenerativeGrammar[L](productions: LogPairedDoubleCounter[L,Rule[L]]) extends Grammar[L] {

  private val leftChildBinaryRules = LogPairedDoubleCounter[L,BinaryRule[L]]();
  private val childUnaryParents = LogPairedDoubleCounter[L,UnaryRule[L]]();
  private val unaryRules = LogPairedDoubleCounter[L,UnaryRule[L]]();
  private val binaryRules = LogPairedDoubleCounter[L,BinaryRule[L]]();

  val index: Index[L] = {
    val index = Index[L]();
    for((a,ctr) <- productions.rows;
        (prod,score) <- ctr) {
      assert(score <= 0,(score,a,prod)+"");
      prod match {
        case u@UnaryRule(_,b) =>
          childUnaryParents(b,u) = score;
          unaryRules(a,u) = score;
          index.index(a);
          index.index(b);
        case br@BinaryRule(_,b,c) =>
          leftChildBinaryRules(b,br) = score;
          binaryRules(a,br) = score;
          index.index(a)
          index.index(b);
          index.index(c);
      }
    }
    index
  }

  private val indexedUnaryRulesByChild:Array[Vector] = fillArray(mkVector(Double.NegativeInfinity));
  for( ((a,UnaryRule(_,b)),score) <- unaryRules) {
    try {
      indexedUnaryRulesByChild(index(b))(index(a)) = score;
    } catch {
      case e =>
        println(index(a),index(b),a,b);
        throw e
    }
  }

  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByLeftChild:Array[SparseArray[Vector]] = (
    fillArray(fillSparseArray(mkVector(Double.NegativeInfinity)))
  );
  for( ((a,BinaryRule(_,b,c)),score) <- binaryRules) {
    indexedBinaryRulesByLeftChild(index(b))(index(c))(index(a)) = score;
  }


  /**
   * Returns pairs of the form ((parent,child),score);
   */
  def unaryRulesByChild(c: L) = {
    assert(c != null);
    childUnaryParents(c).iterator;
  }


  /**
   * Returns a vector of parent index -> score
   */
  def unaryRulesByIndexedChild(c: Int) = {
    indexedUnaryRulesByChild(c);
  }


  /**
   * Returns pairs of the form (child,score);
   */
  def unaryRulesByParent(p: L) = {
    assert(p != null);
    unaryRules(p).iterator;
  }

  /**
   * Returns pairs of the form ( (parent,(left,right)),score);
   */
  def binaryRulesByLeftChild(c: L) = leftChildBinaryRules(c).elements;

  /**
   * Returns a SparseArray[Vector] with RightIndex -> ParentIndex -> Score
   */
  def binaryRulesByIndexedLeftChild(b: Int) = indexedBinaryRulesByLeftChild(b);


  /**
   * Returns pairs of the form ( (lchild,rchild),
   */
  def binaryRulesByParent(p: L) = binaryRules(p).elements;
}