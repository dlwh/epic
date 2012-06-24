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
import breeze.collection.mutable.TriangularArray
import LabelScoreArray._
import java.util.Arrays
import collection.mutable.BitSet

/**
 * A LabelScoreArray is just a triangular array whose entries are arrays. Admits efficient
 * iteration over "on" elements in an (i, j) index.
 * @author dlwh
 */
@SerialVersionUID(2)
class LabelScoreArray[L](length: Int, grammarSize: Int, fill: Double) extends Serializable {

  // (begin,end) -> label ->  refinement -> score
  final val score: Array[Array[Array[Double]]] = new Array[Array[Array[Double]]](TriangularArray.arraySize(length+1))
  final val enteredLabels: Array[BitSet] = mkBitSetArray(TriangularArray.arraySize(length+1))


  final val enteredRefinements: Array[Array[BitSet]] = mkRefinementArray(TriangularArray.arraySize(length+1),grammarSize)

  /**
   * Same as labelScore
   * @return
   */
  final def apply(begin: Int, end: Int, label: Int, ref: Int) = labelScore(begin, end, label, ref)

  /**
   * Returns the score of this (labe,refinement) pair over the span (begin,end)
   * @return
   */
  @inline final def labelScore(begin: Int, end: Int, label: Int, ref: Int):Double = {
    val ind = TriangularArray.index(begin, end)
    if (score(ind) eq null) Double.NegativeInfinity
    else if (score(ind)(label) eq null) Double.NegativeInfinity
    else score(ind)(label)(ref)
  }

  final def enteredLabelIndexes(begin: Int, end: Int): Iterator[Int] = {
    enteredLabels(TriangularArray.index(begin, end)).iterator
  }

  final def enteredLabelRefinements(begin: Int, end: Int, label: Int) = {
    enteredRefinements(TriangularArray.index(begin, end))(label).iterator
  }

  final def enteredLabelScores(begin: Int, end: Int) = {
    val scoreArray = score(TriangularArray.index(begin, end))
    if(scoreArray eq null) Iterator.empty
    else enteredLabels(TriangularArray.index(begin, end)).iterator.map { i => (i, scoreArray(i))}
  }

  protected final def ensureLabelArray(begin: Int, end: Int, parent: Int, numRefinements: Int):Array[Double] = {
    val index = TriangularArray.index(begin, end)
    var arr = score(index)
    if(arr eq null) {
      score(index) = new Array[Array[Double]](grammarSize)
      arr = score(index)
    }
    if(arr(parent) eq null) {
      arr(parent) = LabelScoreArray.mkGrammarVector(numRefinements, fill)
    }
    arr(parent)
  }
}

object LabelScoreArray {
  @inline
  def mkGrammarVector(grammarSize: Int, fill: Double) = {
    val arr = new Array[Double](grammarSize)
    Arrays.fill(arr, fill)
    arr
  }

  @inline def mkBitSetArray(grammarSize: Int) = {
    // too slow :(
    // Array.fill(grammarSize)(new collection.mutable.BitSet())
    val arr = new Array[collection.mutable.BitSet](grammarSize)
    var i = 0
    while (i < arr.length) {
      arr(i) = new collection.mutable.BitSet()
      i += 1
    }
    arr
  }


  @inline
  def mkRefinementArray(length: Int, grammarSize: Int): Array[Array[BitSet]] = {
    val arr = new Array[Array[BitSet]](length)
    var i = 0
    while (i < arr.length) {
      arr(i) = mkBitSetArray(grammarSize)
      i += 1
    }
    arr
  }
}