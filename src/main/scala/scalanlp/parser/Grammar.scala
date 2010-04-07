package scalanlp.parser
/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/



import scalala.Scalala._;
import scalala.tensor._;
import scalanlp.data.VectorBroker
import scalanlp.collection.mutable.SparseArray;
import scalanlp.counters.LogCounters._;
import scalanlp.util.Index;

sealed abstract class Rule[+L] { def parent: L; def children: Seq[L] }
final case class BinaryRule[+L](parent: L, left: L, right: L) extends Rule[L] {
  def children = Seq(left,right);
}
final case class UnaryRule[+L](parent: L, child: L) extends Rule[L] {
  def children = Seq(child);
}

trait Grammar[L] extends VectorBroker[L] {
  def unaryRulesByChild(c: L): Iterator[(UnaryRule[L],Double)];
  def unaryRulesByParent(p: L): Iterator[(UnaryRule[L],Double)];
  def binaryRulesByLeftChild(c: L): Iterator[(BinaryRule[L],Double)];
  /**
   * Returns a vector of parent index -> score
   */
  def unaryRulesByIndexedChild(c: Int): Vector;

  /**
   * Returns a SparseArray[Vector] with RightIndex -> ParentIndex -> Score
   */
  def binaryRulesByIndexedLeftChild(b: Int): SparseArray[Vector];

  /**
   * Returns a SparseArray[Vector] with LeftIndex -> ParentIndex -> Score
   */
  def binaryRulesByIndexedRightChild(c: Int): SparseArray[Vector];
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
      assert(score <= 0,(score,a,prod)+" " + ctr);
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

  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByRightChild:Array[SparseArray[Vector]] = (
    fillArray(fillSparseArray(mkVector(Double.NegativeInfinity)))
  );
  for( ((a,BinaryRule(_,b,c)),score) <- binaryRules) {
    indexedBinaryRulesByLeftChild(index(c))(index(b))(index(a)) = score;
  }

  /**
   * Returns pairs of the form ((parent,child),score);
   */
  def unaryRulesByChild(c: L) = {
    assert(c != null);
    childUnaryParents(c).iterator;
  }


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

  def binaryRulesByIndexedLeftChild(b: Int) = indexedBinaryRulesByLeftChild(b);

  def binaryRulesByIndexedRightChild(c: Int): SparseArray[Vector] = indexedBinaryRulesByRightChild(c);

  /**
   * Returns pairs of the form ( (lchild,rchild),
   */
  def binaryRulesByParent(p: L) = binaryRules(p).elements;
}