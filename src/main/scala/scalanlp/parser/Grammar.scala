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

import scalanlp.util.Index
import scalala.tensor.{Counter2,::}
import scalanlp.serialization.{DataSerialization}
import java.io.{DataInput, DataOutput}
import scalanlp.serialization.DataSerialization.ReadWritable
import scalala.library.Library._
import collection.mutable.ArrayBuffer
import scalanlp.collection.mutable.{OpenAddressHashArray}

@SerialVersionUID(2)
sealed trait Grammar[L] extends Encoder[Rule[L]] with Serializable {
  override val index: Index[Rule[L]]
  val labelIndex: Index[L];
  def labelEncoder  = Encoder.fromIndex(labelIndex)
  val unaryRules: Counter2[L, UnaryRule[L], Double]
  val binaryRules: Counter2[L, BinaryRule[L], Double]
  // don't mutate me!
  val indexedRules: Array[Rule[Int]];
  // don't mutate me!
  val ruleScoreArray: Array[Double];

  def ruleIndex(a: Int, b: Int, c: Int):Int
  def ruleIndex(a: Int, b: Int):Int

  def binaryRulesWithParent(l: L) = binaryRules(l,::).pairsIteratorNonZero
  // Rule Index
  def indexedBinaryRulesWithParent(l: Int):Array[Int]
  // Rule Index
  def indexedUnaryRulesWithChild(l: Int):Array[Int]
  // Rule Index
  def indexedUnaryRulesWithParent(l: Int):Array[Int]

  final def ruleScore(r: Int):Double = ruleScoreArray(r)
  def ruleScore(a: Int, b: Int, c: Int):Double = ruleScoreArray(ruleIndex(a,b,c))
  def ruleScore(a: Int, b: Int):Double = ruleScoreArray(ruleIndex(a,b))

  def parent(r: Int) = indexedRules(r).parent
  def leftChild(r: Int) = indexedRules(r).asInstanceOf[BinaryRule[Int]].left
  def rightChild(r: Int) = indexedRules(r).asInstanceOf[BinaryRule[Int]].right
  def child(r: Int) = indexedRules(r).asInstanceOf[UnaryRule[Int]].child


  def maxNumBinaryRulesForParent: Int

  /**
   * Returns true if the label has no productions with it on the LHS.
   */
  def isPreterminal(label: Int): Boolean = {
    indexedBinaryRulesWithParent(label).size == 0;
  }

  final def ruleScore(r: Rule[L]) = ruleScoreArray(index(r))
}

object Grammar {
  def apply[L](binaryProductions: Counter2[L,BinaryRule[L],Double],
               unaryProductions: Counter2[L,UnaryRule[L],Double]):Grammar[L] = {
    val (index: Index[L],ruleIndex: Index[Rule[L]]) = {
      val index = Index[L]();
      val ruleIndex = Index[Rule[L]]()
      for(((a,br),score) <- binaryProductions.nonzero.pairs) {
        if(score != Double.NegativeInfinity) {
          index.index(a)
          index.index(br.left);
          index.index(br.right);
          ruleIndex.index(br)
        }
      }
      for(((a,u),score) <- unaryProductions.nonzero.pairs) {
        if(score != Double.NegativeInfinity) {
          index.index(a);
          index.index(u.child);
          ruleIndex.index(u)
        }
      }
      index -> ruleIndex
    }
    apply(index,ruleIndex,binaryProductions,unaryProductions)
  }

  def apply[L](labelIndex: Index[L],
               ruleIndex: Index[Rule[L]],
               binaryProductions: Counter2[L,BinaryRule[L],Double],
               unaryProductions: Counter2[L,UnaryRule[L],Double]):Grammar[L] = {
    val li = labelIndex
    val ri = ruleIndex
    new Grammar[L] {
      val index = ri
      val labelIndex = li
      val unaryRules = unaryProductions
      val binaryRules = binaryProductions
      val indexedRules = for ( r <- index.toArray) yield r match {
        case BinaryRule(a,b,c) => BinaryRule(labelIndex(a),labelIndex(b),labelIndex(c)):Rule[Int]
        case UnaryRule(a,b) => UnaryRule(labelIndex(a),labelIndex(b)):Rule[Int]
      }
      val ruleScoreArray = for(r <- index.toArray) yield r match {
        case r@BinaryRule(a,_,_) => binaryProductions(a,r)
        case r@UnaryRule(a,_) => unaryProductions(a,r)
      }

      val binaryRuleTable = new OpenAddressHashArray[Int](labelIndex.size * labelIndex.size * labelIndex.size)
      val unaryRuleTable = new OpenAddressHashArray[Int](labelIndex.size * labelIndex.size)

      private val (binaryRulesByParent,unaryRulesByParent,binaryRulesByLeftChild,binaryRulesByRightChild,unaryRulesByChild) = {
        val binaryRulesByParent = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
        val unaryRulesByParent = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
        val binaryRulesByLeftChild = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
        val binaryRulesByRightChild = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
        val unaryRulesByChild = Array.fill(labelIndex.size)(new ArrayBuffer[Int]())
        for ( (r,i) <- indexedRules.zipWithIndex) r match {
          case BinaryRule(p,l,rc) =>
            binaryRulesByParent(p) += i
            binaryRulesByLeftChild(l) += i
            binaryRulesByRightChild(rc) += i
            binaryRuleTable(rc + labelIndex.size * (l + labelIndex.size * p)) = i
          case UnaryRule(p,c) =>
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


      def ruleIndex(a: Int, b: Int, c: Int) = binaryRuleTable(c + labelIndex.size * (b + labelIndex.size * a))
      def ruleIndex(a: Int, b: Int) = unaryRuleTable(b + labelIndex.size * a)


      def indexedBinaryRulesWithParent(l: Int) = binaryRulesByParent(l)
      def indexedUnaryRulesWithParent(l: Int) = unaryRulesByParent(l)
      def indexedUnaryRulesWithChild(l: Int) = unaryRulesByChild(l)

      val maxNumBinaryRulesForParent = {
        for(a <- binaryRules.domain._1) yield binaryRules(a,::).size
      }.max
    }

  }

  def zero[L](grammar: Grammar[L]) = {
    apply(grammar.labelIndex,grammar.index,grammar.binaryRules :* 0.0,grammar.unaryRules :* 0.0)
  }

  implicit def grammarIsReadWritable[L:DataSerialization.ReadWritable]: ReadWritable[Grammar[L]]  = {
    new DataSerialization.ReadWritable[Grammar[L]] {
      def write(sink: DataOutput, g: Grammar[L]) {
        DataSerialization.write(sink, g.labelIndex)
        DataSerialization.write(sink, g.index)
        DataSerialization.write(sink, g.unaryRules)
        DataSerialization.write(sink, g.binaryRules)
      }

      def read(source: DataInput) = {
        val i = DataSerialization.read[Index[L]](source)
        val ri = DataSerialization.read[Index[Rule[L]]](source)
        val u = DataSerialization.read[Counter2[L,UnaryRule[L],Double]](source)
        val b = DataSerialization.read[Counter2[L,BinaryRule[L],Double]](source)
        Grammar(i,ri,b,u)
      }
    }
  }

}
