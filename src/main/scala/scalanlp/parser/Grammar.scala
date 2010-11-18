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

import scalala.Scalala._
import java.io.DataOutput;
import scalala.tensor._;
import scalanlp.util.Encoder;
import scalala.tensor.sparse.SparseVector
import scalanlp.collection.mutable.SparseArray;
import scalala.tensor.adaptive.AdaptiveVector
import scalala.tensor.counters.LogCounters._;
import scalanlp.util.Index;

sealed abstract trait Rule[@specialized(Int) +L] { def parent: L; def children: Seq[L] }
final case class BinaryRule[@specialized(Int) +L](parent: L, left: L, right: L) extends Rule[L] {
  def children = Seq(left,right);
}
final case class UnaryRule[@specialized(Int) +L](parent: L, child: L) extends Rule[L] {
  def children = Seq(child);
}

@serializable
@SerialVersionUID(1)
trait Grammar[L] extends Encoder[L] {
  override val index: Index[L];
  def unaryRulesByChild(c: L): Iterator[(UnaryRule[L],Double)];
  def unaryRulesByParent(p: L): Iterator[(UnaryRule[L],Double)];
  def binaryRulesByLeftChild(c: L): Iterator[(BinaryRule[L],Double)];
  /**
   * Returns a vector of parent index -> score
   */
  def unaryRulesByIndexedChild(c: Int): SparseVector;

  /**
   * Returns a vector of child index -> score
   */
  def unaryRulesByIndexedParent(p: Int): SparseVector;

  /**
   * Returns true if the label has no productions with it on the LHS.
   */
  def isPreterminal(label: Int): Boolean = {
    unaryRulesByIndexedParent(label).used == 0 && binaryRulesByIndexedParent(label).size == 0;
  }

  /**
   * Returns a SparseArray[Vector] with RightIndex -> ParentIndex -> Score
   */
  def binaryRulesByIndexedLeftChild(b: Int): SparseArray[SparseVector];

  /** Returns rules in lchild -> rchild -> parent -> score form */
  def allBinaryRules:SparseArray[SparseArray[SparseVector]];

  /** Returns rules in child -> parent -> score form */
  def allUnaryRules:SparseArray[SparseVector];

  /** Returns the score of a binary rule */
  def binaryRuleScore(a: Int, b:Int, c: Int): Double;
  /** Returns the score of a unary rule */
  def unaryRuleScore(a: Int, b: Int):Double

  /**
   * Returns a SparseArray[Vector] with LeftIndex -> RightIndex -> Score
   */
  def binaryRulesByIndexedParent(a: Int): SparseArray[SparseVector];

  /**
   * Returns a SparseArray[Vector] with LeftIndex -> ParentIndex -> Score
   */
  def binaryRulesByIndexedRightChild(c: Int): SparseArray[SparseVector];
}

/*
object Grammar {
  import scalanlp.serialization.DataSerialization._;
  import scalanlp.serialization.DataSerialization
  implicit def grammarIsWritable[L:Writable]: Writable[Grammar[L]] = new Writable[Grammar[L]] {
    def write(out: DataOutput, g: Grammar[L]) = {
      // Grammar consists of an index, unary rules, and binary rules.
      DataSerialization.write(out, g.index);
      // Unary format: (parent index,<sparse vec of unary rules>)*, -1
      for(i <- 0 until g.index.size) {
        val vec = g.unaryRulesByIndexedParent(i);
        if(g.used != 0) {
          DataSerialization.write(out, i);
          DataSerialization.write(out, vec);
        }
      }
      DataSerialization.write(-1);
      // Binary format: (parent index,(lchild index, sparsevec of rchild,score)


    }
  }
}
*/

/**
 * Given a counter of productions that has been log-normalized by rows,
 * creates a grammar. Simple simple.
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
      if(score != Double.NegativeInfinity) {
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
    }
    index
  }

  private val indexedUnaryRulesByChild:SparseArray[SparseVector] = fillSparseArray(mkSparseVector(Double.NegativeInfinity));
  for( ((a,UnaryRule(_,b)),score) <- unaryRules) {
    indexedUnaryRulesByChild.getOrElseUpdate(index(b))(index(a)) = score;
  }

  private val indexedUnaryRulesByParent:SparseArray[SparseVector] = fillSparseArray(mkSparseVector(Double.NegativeInfinity));
  for( ((a,UnaryRule(_,b)),score) <- unaryRules) {
    indexedUnaryRulesByParent.getOrElseUpdate(index(a))(index(b)) = score;
  }


  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByLeftChild:SparseArray[SparseArray[SparseVector]] = (
    fillSparseArray(fillSparseArray(mkSparseVector(Double.NegativeInfinity)))
  );
  for( ((a,BinaryRule(_,b,c)),score) <- binaryRules) {
    indexedBinaryRulesByLeftChild.getOrElseUpdate(index(b)).getOrElseUpdate(index(c))(index(a)) = score;
  }

  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByRightChild:SparseArray[SparseArray[SparseVector]] = (
    fillSparseArray(fillSparseArray(mkSparseVector(Double.NegativeInfinity)))
  );
  for( ((a,BinaryRule(_,b,c)),score) <- binaryRules) {
    indexedBinaryRulesByRightChild.getOrElseUpdate(index(c)).getOrElseUpdate(index(b))(index(a)) = score;
  }

  // Mapping is Parent -> Left Child -> Right Child ->  Score
  private val indexedBinaryRulesByParent:SparseArray[SparseArray[SparseVector]] = (
    fillSparseArray(fillSparseArray(mkSparseVector(Double.NegativeInfinity)))
  );
  for( ((a,BinaryRule(_,b,c)),score) <- binaryRules) {
    indexedBinaryRulesByParent.getOrElseUpdate(index(a)).getOrElseUpdate(index(b))(index(c)) = score;
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

  def unaryRulesByIndexedParent(p: Int) = {
    indexedUnaryRulesByParent(p);
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
  def binaryRulesByLeftChild(c: L) = leftChildBinaryRules(c).iterator;

  def allBinaryRules = indexedBinaryRulesByLeftChild;
  def allUnaryRules = indexedUnaryRulesByChild;

  def binaryRulesByIndexedLeftChild(b: Int) = indexedBinaryRulesByLeftChild(b);

  def binaryRulesByIndexedRightChild(c: Int): SparseArray[SparseVector] = indexedBinaryRulesByRightChild(c);

  def binaryRuleScore(a: Int, b: Int, c: Int) = indexedBinaryRulesByParent(a)(b)(c);
  def unaryRuleScore(a: Int, b: Int) = indexedUnaryRulesByParent(a)(b);

  /**
   * Returns pairs of the form (lchild,rchild),
   */
  def binaryRulesByParent(p: L) = binaryRules(p).iterator;
  /** b, c **/
  def binaryRulesByIndexedParent(a: Int): SparseArray[SparseVector] = indexedBinaryRulesByParent(a);
}