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
import scalanlp.serialization.{DataSerialization}
import java.io.{DataInput, DataOutput}
import scalanlp.serialization.DataSerialization.ReadWritable
import scalala.library.Library._

@SerialVersionUID(2)
sealed trait Grammar[L] extends Encoder[L] with Serializable {
  override val index: Index[L];
  val unaryRules: Counter2[L,UnaryRule[L], Double]
  val binaryRules: Counter2[L,BinaryRule[L], Double]
  lazy val maxNumBinaryRulesForParent: Int = {
    for(a <- binaryRules.domain._1) yield binaryRules(a,::).size
  } max
  def unaryRulesByChild(c: L): Iterator[(UnaryRule[L],Double)];
  def unaryRulesByParent(p: L): Iterator[(UnaryRule[L],Double)];
  def binaryRulesByLeftChild(c: L): Iterator[(BinaryRule[L],Double)];
  def binaryRulesByParent(p: L): Iterator[(BinaryRule[L],Double)]
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

  /** Returns rules in parent -> lchild -> rchild -> score form*/
  def allBinaryRulesByParent:SparseArrayMap[SparseArrayMap[OldSparseVector]];

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

object Grammar {
  def apply[L](binaryProductions: Counter2[L,BinaryRule[L],Double],
               unaryProductions: Counter2[L,UnaryRule[L],Double]):Grammar[L] = {
    val index: Index[L] = {
      val index = Index[L]();
      for(((a,br),score) <- binaryProductions.nonzero.pairs) {
        if(score != Double.NegativeInfinity) {
          index.index(a)
          index.index(br.left);
          index.index(br.right);
        }
      }
      for(((a,u),score) <- unaryProductions.nonzero.pairs) {
        if(score != Double.NegativeInfinity) {
          index.index(a);
          index.index(u.child);
        }
      }
      index
    }
    apply(index,binaryProductions,unaryProductions)
  }

  def apply[L](index: Index[L],
               binaryProductions: Counter2[L,BinaryRule[L],Double],
               unaryProductions: Counter2[L,UnaryRule[L],Double]):Grammar[L] = {
    val ind = index
    new Grammar[L] {
      val index = ind
      private val leftChildBinaryRules = Counter2[L,BinaryRule[L], Double]();
      private val childUnaryParents = Counter2[L,UnaryRule[L], Double]();
      val unaryRules = Counter2[L,UnaryRule[L], Double]();
      val binaryRules = Counter2[L,BinaryRule[L], Double]();

      for(((a,br),score) <- binaryProductions.nonzero.pairs) {
        if(score != Double.NegativeInfinity) {
          leftChildBinaryRules(br.left,br) = score;
          binaryRules(a,br) = score;
        }
      }
      for(((a,u),score) <- unaryProductions.nonzero.pairs) {
        if(score != Double.NegativeInfinity) {
          childUnaryParents(u.child,u) = score;
          unaryRules(a,u) = score;
        }
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
        unaryRules(p, ::).nonzero.pairs.iterator;
      }

      /**
       * Returns pairs of the form ( (parent,(left,right)),score);
       */
      def binaryRulesByLeftChild(c: L) = leftChildBinaryRules(c, ::).nonzero.pairs.iterator;

      def allBinaryRules = indexedBinaryRulesByLeftChild;
      def allBinaryRulesByParent = indexedBinaryRulesByParent;
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

  }

  def zero[L](grammar: Grammar[L]) = {
    apply(grammar.index,grammar.binaryRules :* 0.0,grammar.unaryRules :* 0.0)
  }

  implicit def grammarIsReadWritable[L:DataSerialization.ReadWritable]: ReadWritable[Grammar[L]]  = {
    new DataSerialization.ReadWritable[Grammar[L]] {
      def write(sink: DataOutput, g: Grammar[L]) {
        DataSerialization.write(sink, g.index)
        DataSerialization.write(sink, g.unaryRules)
        DataSerialization.write(sink, g.binaryRules)
      }

      def read(source: DataInput) = {
        val i = DataSerialization.read[Index[L]](source)
        val u = DataSerialization.read[Counter2[L,UnaryRule[L],Double]](source)
        val b = DataSerialization.read[Counter2[L,BinaryRule[L],Double]](source)
        Grammar(i,b,u)
      }
    }
  }

}
