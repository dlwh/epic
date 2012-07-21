package epic.parser
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import math._
import java.util.Arrays
import breeze.util.Index
import breeze.collection.mutable.TriangularArray
import breeze.numerics.logSum
import collection.mutable.BitSet
import collection.immutable

@SerialVersionUID(3)
abstract class ParseChart[L](val index: Index[L],
                             // label => number of refinements
                             refinementsFor: Array[Int],
                             val length: Int,
                             sparsity: ParseChart.SparsityPattern) extends Serializable {
  private val grammarSize = index.size

  final val top = new ChartScores(sparsity.activeLabelsTop _)
  final val bot = new ChartScores(sparsity.activeLabelsBot _)

  /**
   * A ChartScores is just a triangular array whose entries are arrays. Admits efficient
   * iteration over "on" elements in an (i, j) index.
   * @author dlwh
   */
  final class ChartScores private[ParseChart](sparsityLabel: (Int,Int)=>immutable.BitSet) {

    import ParseChart._

    /** (begin,end) -> label ->  refinement -> score */
    // fill in arrays for spans we might touch
    val score: Array[Array[Array[Double]]] = Array.tabulate(TriangularArray.arraySize(length+1)){i =>
      if(sparsity.activeTriangularIndices.contains(i)) {
        new Array[Array[Double]](index.size)
      } else {
        null
      }
    }

    // now fill in components of touch spans
    for(b <- 0 until length; e <- b to length) {
      val ind = TriangularArray.index(b, e)
      val arr = score(ind)
      if(arr != null) {
        for(l <- sparsityLabel(b, e)) {
          arr(l) = mkGrammarVector(refinementsFor(l), zero)
        }
      }
    }


    /** (begin,end) -> which labels are on */
    val enteredLabels: Array[BitSet] = mkBitSetArray(TriangularArray.arraySize(length+1))
    /** (begin,end) -> label -> which refinements of label are on */
    val enteredRefinements: Array[Array[BitSet]] = mkRefinementArray(TriangularArray.arraySize(length+1),grammarSize)

    /** Same as labelScore */
    def apply(begin: Int, end: Int, label: Int, ref: Int) = labelScore(begin, end, label, ref)

    /**
     * Returns the score of this (label,refinement) pair over the span (begin,end)
     * @return
     */
    def labelScore(begin: Int, end: Int, label: Int, ref: Int):Double = {
      val ind = TriangularArray.index(begin, end)
      if (score(ind) eq null) Double.NegativeInfinity
      else if (score(ind)(label) eq null) Double.NegativeInfinity
      else score(ind)(label)(ref)
    }

    @inline def labelScore(begin: Int, end: Int, parent: L, ref: Int):Double = {
      labelScore(begin, end, index(parent), ref)
    }

    def enteredLabelIndexes(begin: Int, end: Int): Iterator[Int] = {
      enteredLabels(TriangularArray.index(begin, end)).iterator
    }

    def isLabelEntered(begin: Int, end: Int, l: Int): Boolean = {
      enteredLabels(TriangularArray.index(begin,end)).contains(l)
    }

    def enteredLabelRefinements(begin: Int, end: Int, label: Int) = {
      enteredRefinements(TriangularArray.index(begin, end))(label).iterator
    }

    def enteredLabelScores(begin: Int, end: Int) = {
      val scoreArray = score(TriangularArray.index(begin, end))
      if(scoreArray eq null) Iterator.empty
      else enteredLabels(TriangularArray.index(begin, end)).iterator.map { i => (i, scoreArray(i))}
    }


    def rawEnter(begin: Int, end: Int, parent: Int, ref: Int, w: Double) = {
      val arrx = score(TriangularArray.index(begin, end))
      val arr = arrx(parent)
      val oldScore = arr(ref)
      val newScore = sum(oldScore, w)
      arr(ref) = newScore
      oldScore
    }


    def enter(begin: Int, end: Int, parent: Int, ref: Int, w: Double) {
      val oldScore =  rawEnter(begin, end, parent, ref, w)

      if(oldScore == zero) {
        val index = TriangularArray.index(begin, end)
        updateExtents(index, parent, ref, begin, end)
      }
    }

    def enterSum(begin: Int, end: Int, parent: Int, ref: Int, w: Array[Double], length: Int) = {
      enter(begin, end, parent, ref, sum(w,length))
    }

    /**
     * Updates the extent arrays for a given set of refinements
     */
    private def updateExtents(index: Int, parent: Int, ref: Int, begin: Int, end: Int) {
      enteredLabels(index) += parent
      enteredRefinements(index)(parent) += ref

      val narrowLeftEndParent = narrowLeft(end)(parent)
      val wideLeftEnd = wideLeft(end)
      narrowLeftEndParent(ref) = math.max(begin, narrowLeftEndParent(ref))
      wideLeftEnd(parent)(ref) = math.min(begin, wideLeftEnd(parent)(ref))

      val wideRightBegin = wideRight(begin)
      val narrowRightBegin = narrowRight(begin)
      wideRightBegin(parent)(ref) = math.max(end, wideRightBegin(parent)(ref))
      narrowRightBegin(parent)(ref) = math.min(end, narrowRightBegin(parent)(ref))

      coarseNarrowLeft(end)(parent) = math.max(begin, coarseNarrowLeft(end)(parent))
      coarseWideLeft(end)(parent) = math.min(begin, coarseWideLeft(end)(parent))
      coarseWideRight(begin)(parent) = math.max(end, coarseWideRight(begin)(parent))
      coarseNarrowRight(begin)(parent) = math.min(end, coarseNarrowRight(begin)(parent))
    }

    /** right most place a left constituent with label l can start and end at position i. (start)(sym)(ref) */
    val narrowLeft: Array[Array[Array[Int]]] = Array.tabulate(length+1, grammarSize){ (i, l) =>
      val arr = new Array[Int](refinementsFor(l))
      Arrays.fill(arr, -1)
      arr
    }
    /** left most place a left constituent with label l can start and end at position i. (start)(sym)(ref) */
    val wideLeft = Array.tabulate[Array[Int]](length+1, grammarSize){ (i, l) =>
      val arr = new Array[Int](refinementsFor(l))
      Arrays.fill(arr, length + 1)
      arr
    }
    /** left most place a right constituent with label l--which starts at position i--can end. (end)(sym)(ref) */
    val narrowRight = Array.tabulate[Array[Int]](length+1, grammarSize){ (i, l) =>
      val arr = new Array[Int](refinementsFor(l))
      Arrays.fill(arr, length + 1)
      arr
    }
    /** right-most place a right constituent with label l--which starts at position i--can end. (end)(sym)(ref) */
    val wideRight = Array.tabulate[Array[Int]](length+1, grammarSize) { (i, l) =>
      val arr = new Array[Int](refinementsFor(l))
      Arrays.fill(arr, -1)
      arr
    }

    /** right most place a left constituent with label l can start and end at position i. (start)(sym) */
    val coarseNarrowLeft = makeCoarseExtentArray(-1)
    /** left most place a left constituent with label l can start and end at position i  (start)(sym) */
    val coarseWideLeft = makeCoarseExtentArray(length+1)
    /** left most place a right constituent with label l--which starts at position i--can end. (end)(sym) */
    val coarseNarrowRight = makeCoarseExtentArray(length+1)
    /** right-most place a right constituent with label l--which starts at position i--can end. (end)(sym)*/
    val coarseWideRight = makeCoarseExtentArray(-1)

    def feasibleSpan(begin: Int, end: Int, b: Int, refB: Int, c: Int, refC: Int) = {
      if(narrowRight(begin)(b) == null || narrowLeft(end)(c) == null) {
        Range(0,0)
      } else {
        val narrowR = narrowRight(begin)(b)(refB)
        val narrowL = narrowLeft(end)(c)(refC)
        var split = math.max(narrowR, wideLeft(end)(c)(refC))
        val endSplit = math.min(wideRight(begin)(b)(refB), narrowL) + 1
        val canBuildThisRule = narrowR < end && narrowL >= narrowR && split <= narrowL && split < endSplit
        if(!canBuildThisRule)
          split = endSplit

        Range(split, endSplit)
      }
    }

    private def makeCoarseExtentArray(value: Int) = {
      val arr = Array.ofDim[Int](length + 1, grammarSize)
      for(arr1 <- arr) {
        Arrays.fill(arr1, value)
      }
      arr
    }

  }

  // requirements: sum(a, b) >= a, \forall b that might be used.
  def sum(a:Double, b: Double):Double
  // possibly faster sum
  def sum(arr: Array[Double], length: Int):Double
  protected final def zero = Double.NegativeInfinity
}


object ParseChart {

  @SerialVersionUID(1L)
  trait SparsityPattern extends Serializable {
    def activeTriangularIndices: immutable.BitSet
    def activeLabelsTop(begin: Int, end: Int): immutable.BitSet
    def activeLabelsBot(begin: Int, end: Int): immutable.BitSet
  }

  object SparsityPattern {
    def noSparsity[L](labels: Index[L], length: Int):SparsityPattern = new SparsityPattern {
      def activeTriangularIndices: immutable.BitSet = {
        immutable.BitSet.empty ++ Range(0, TriangularArray.arraySize(length+1))
      }

      val allLabels = immutable.BitSet.empty ++ Range(0, labels.size)

      def activeLabelsTop(begin: Int, end: Int) = allLabels
      def activeLabelsBot(begin: Int, end: Int) = allLabels
    }
  }

  def apply[L](g: Index[L], refinements: Array[Int], length: Int) = logProb(g, refinements, length)

  @SerialVersionUID(1)
  trait Factory[+Chart[X]<:ParseChart[X]] extends Serializable {
    def apply[L](g: Index[L], refinements: Array[Int], length: Int):Chart[L] = {
      apply(g, refinements, length, SparsityPattern.noSparsity(g, length))
    }

    def apply[L](g: Index[L], refinements: Array[Int], length: Int, sparsity: SparsityPattern):Chart[L]
  }

  // concrete factories:
  object viterbi extends Factory[ViterbiParseChart] {
    def apply[L](g: Index[L], refinements: Array[Int], length: Int, sparsity: SparsityPattern) = {
      new ParseChart(g, refinements, length, sparsity) with Viterbi
    }
  }

  object logProb extends Factory[LogProbabilityParseChart] {
    def apply[L](g: Index[L], refinements: Array[Int], length: Int, sparsity: SparsityPattern) = {
      new ParseChart(g, refinements, length, sparsity) with LogProbability
    }
  }

  private def mkGrammarVector(grammarSize: Int, fill: Double) = {
    val arr = new Array[Double](grammarSize)
    Arrays.fill(arr, fill)
    arr
  }

  private def mkBitSetArray(grammarSize: Int) = {
    val arr = new Array[collection.mutable.BitSet](grammarSize)
    var i = 0
    while (i < arr.length) {
      arr(i) = new collection.mutable.BitSet()
      i += 1
    }
    arr
  }

  private def mkRefinementArray(length: Int, grammarSize: Int): Array[Array[BitSet]] = {
    val arr = new Array[Array[BitSet]](length)
    var i = 0
    while (i < arr.length) {
      arr(i) = mkBitSetArray(grammarSize)
      i += 1
    }
    arr
  }

  trait Viterbi {
    final def sum(a: Double, b: Double) = math.max(a, b)

    def sum(a: Array[Double], length: Int) = {
      var i = 1
      var max =  a(0)
      while(i < length) {
        if(a(i) > max) max = a(i)
        i += 1
      }
      max

    }
  }
  type ViterbiParseChart[L] = ParseChart[L] with Viterbi

  trait LogProbability {
    final def sum(a: Double, b: Double) = {
      // log1p isn't optimized, and I don't really care about accuracy that much
      // scalala.library.Numerics.logSum(a, b)
      if (a == Double.NegativeInfinity) b
      else if (b == Double.NegativeInfinity) a
      else if (a < b) b + log(1+exp(a - b))
      else a + log(1+exp(b - a))
    }
    final def sum(arr: Array[Double], length: Int) = logSum(arr, length)
  }
  type LogProbabilityParseChart[L] = ParseChart[L] with LogProbability

}



