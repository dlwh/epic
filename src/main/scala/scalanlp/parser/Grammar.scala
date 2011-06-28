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



import scalanlp.util.Encoder;
import scalanlp.collection.mutable.SparseArrayMap;
import scalanlp.util.Index
import scalanlp.tensor.sparse.OldSparseVector
import scalala.tensor.{Counter2,::}

sealed trait Rule[@specialized(Int) +L] { def parent: L; def children: Seq[L] }
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
  def unaryRulesByIndexedChild(c: Int): OldSparseVector;

  /**
   * Returns a vector of child index -> score
   */
  def unaryRulesByIndexedParent(p: Int): OldSparseVector;

  /**
   * Returns true if the label has no productions with it on the LHS.
   */
  def isPreterminal(label: Int): Boolean = {
    binaryRulesByIndexedParent(label).size == 0;
  }

  /**
   * Returns a SparseArrayMap[Vector] with RightIndex -> ParentIndex -> Score
   */
  def binaryRulesByIndexedLeftChild(b: Int): SparseArrayMap[OldSparseVector];

  /** Returns rules in lchild -> rchild -> parent -> score form */
  def allBinaryRules:SparseArrayMap[SparseArrayMap[OldSparseVector]];

  /** Returns rules in child -> parent -> score form */
  def allUnaryRules:SparseArrayMap[OldSparseVector];

  /** Returns the score of a binary rule */
  def binaryRuleScore(a: Int, b:Int, c: Int): Double;
  /** Returns the score of a unary rule */
  def unaryRuleScore(a: Int, b: Int):Double

  /**
   * Returns a SparseArrayMap[Vector] with LeftIndex -> RightIndex -> Score
   */
  def binaryRulesByIndexedParent(a: Int): SparseArrayMap[OldSparseVector];

  /**
   * Returns a SparseArrayMap[Vector] with LeftIndex -> ParentIndex -> Score
   */
  def binaryRulesByIndexedRightChild(c: Int): SparseArrayMap[OldSparseVector];
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
@SerialVersionUID(925230863821585211L)
class GenerativeGrammar[L](binaryProductions: Counter2[L,BinaryRule[L],Double],
                           unaryProductions: Counter2[L,UnaryRule[L],Double]) extends Grammar[L] {

  private val leftChildBinaryRules = Counter2[L,BinaryRule[L], Double]();
  private val childUnaryParents = Counter2[L,UnaryRule[L], Double]();
  private val unaryRules = Counter2[L,UnaryRule[L], Double]();
  private val binaryRules = Counter2[L,BinaryRule[L], Double]();

  val index: Index[L] = {
    val index = Index[L]();
    for(((a,br),score) <- binaryProductions.nonzero.pairs) {
      if(score != Double.NegativeInfinity) {
        leftChildBinaryRules(br.left,br) = score;
        binaryRules(a,br) = score;
        index.index(a)
        index.index(br.left);
        index.index(br.right);
      }
    }
    for(((a,u),score) <- unaryProductions.nonzero.pairs) {
      if(score != Double.NegativeInfinity) {
        childUnaryParents(u.child,u) = score;
        unaryRules(a,u) = score;
        index.index(a);
        index.index(u.child);
      }
    }
    index
  }

  private val indexedUnaryRulesByChild:SparseArrayMap[OldSparseVector] = fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity));
  for( ((a,UnaryRule(_,b)),score) <- unaryRules.nonzero.pairs) {
    indexedUnaryRulesByChild.getOrElseUpdate(index(b))(index(a)) = score;
  }

  private val indexedUnaryRulesByParent:SparseArrayMap[OldSparseVector] = fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity));
  for( ((a,UnaryRule(_,b)),score) <- unaryRules.nonzero.pairs) {
    indexedUnaryRulesByParent.getOrElseUpdate(index(a))(index(b)) = score;
  }


  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByLeftChild:SparseArrayMap[SparseArrayMap[OldSparseVector]] = (
    fillSparseArrayMap(fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity)))
  );
  for( ((a,BinaryRule(_,b,c)),score) <- binaryRules.nonzero.pairs) {
    indexedBinaryRulesByLeftChild.getOrElseUpdate(index(b)).getOrElseUpdate(index(c))(index(a)) = score;
  }

  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByRightChild:SparseArrayMap[SparseArrayMap[OldSparseVector]] = (
    fillSparseArrayMap(fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity)))
  );
  for( ((a,BinaryRule(_,b,c)),score) <- binaryRules.nonzero.pairs) {
    indexedBinaryRulesByRightChild.getOrElseUpdate(index(c)).getOrElseUpdate(index(b))(index(a)) = score;
  }

  // Mapping is Parent -> Left Child -> Right Child ->  Score
  private val indexedBinaryRulesByParent:SparseArrayMap[SparseArrayMap[OldSparseVector]] = (
    fillSparseArrayMap(fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity)))
  );
  for( ((a,BinaryRule(_,b,c)),score) <- binaryRules.nonzero.pairs) {
    indexedBinaryRulesByParent.getOrElseUpdate(index(a)).getOrElseUpdate(index(b))(index(c)) = score;
  }

  /**
   * Returns pairs of the form ((parent,child),score);
   */
  def unaryRulesByChild(c: L) = {
    assert(c != null);
    childUnaryParents(c, ::).nonzero.pairs.iterator
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
    unaryRules(p, ::).nonzero.pairs.iterator;
  }

  /**
   * Returns pairs of the form ( (parent,(left,right)),score);
   */
  def binaryRulesByLeftChild(c: L) = leftChildBinaryRules(c, ::).nonzero.pairs.iterator;

  def allBinaryRules = indexedBinaryRulesByLeftChild;
  def allUnaryRules = indexedUnaryRulesByChild;

  def binaryRulesByIndexedLeftChild(b: Int) = indexedBinaryRulesByLeftChild(b);

  def binaryRulesByIndexedRightChild(c: Int): SparseArrayMap[OldSparseVector] = indexedBinaryRulesByRightChild(c);

  def binaryRuleScore(a: Int, b: Int, c: Int) = indexedBinaryRulesByParent(a)(b)(c);
  def unaryRuleScore(a: Int, b: Int) = indexedUnaryRulesByParent(a)(b);

  /**
   * Returns pairs of the form (lchild,rchild),
   */
  def binaryRulesByParent(p: L) = binaryRules(p, ::).nonzero.pairs.iterator;
  /** b, c **/
  def binaryRulesByIndexedParent(a: Int): SparseArrayMap[OldSparseVector] = indexedBinaryRulesByParent(a);
}