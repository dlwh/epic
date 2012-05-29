package scalanlp.parser
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

import scalanlp.collection.mutable.TriangularArray

import math._
import java.util.Arrays
import scalanlp.util.Index

@SerialVersionUID(3)
abstract class ParseChart[L](val index: Index[L],
                             // label => number of refinements
                             refinementsFor: Array[Int],
                             val length: Int) extends Serializable {
  private val grammarSize = index.size

  final val top = new ChartScores()
  final val bot = new ChartScores()

  final class ChartScores private[ParseChart]() extends LabelScoreArray[L](length, grammarSize, zero) {

    private[ParseChart] def scoreArray = score

    def enter(begin: Int, end: Int, parent: Int, ref: Int, w: Double):Boolean = {
      val index = TriangularArray.index(begin, end)
      val arr = ensureLabelArray(begin, end, parent, refinementsFor(parent))
      val oldScore = arr(ref)
      val newScore = sum(oldScore, w)
      arr(ref) = newScore

      if(oldScore == zero) {
        updateExtents(index, parent, ref, begin, end)
      }
      newScore > oldScore
    }

    def enterSum(begin: Int, end: Int, parent: Int, ref: Int, w: Array[Double], length: Int):Boolean = {
      enter(begin, end, parent, ref, sum(w,length))
    }

    def labelScore(begin: Int, end: Int, parent: L, ref: Int):Double = {
      labelScore(begin, end, index(parent), ref)
    }

    /**
     * Updates the extent arrays for a given set of refinements
     */
    private def updateExtents(index: Int, parent: Int, ref: Int, begin: Int, end: Int) {
      enteredLabels(index) += parent
      enteredRefinements(index)(parent) += ref

      val narrowLeftEnd = narrowLeft(end)
      val wideLeftEnd = wideLeft(end)
      if(narrowLeftEnd(parent) eq null) {
        narrowLeftEnd(parent) = new Array[Int](refinementsFor(parent))
        Arrays.fill(narrowLeftEnd(parent), -1)
        wideLeftEnd(parent) = new Array[Int](refinementsFor(parent))
        Arrays.fill(wideLeftEnd(parent), length+1)
      }
      narrowLeftEnd(parent)(ref) = math.max(begin, narrowLeftEnd(parent)(ref))
      wideLeftEnd(parent)(ref) = math.min(begin, wideLeftEnd(parent)(ref))

      val wideRightBegin = wideRight(begin)
      val narrowRightBegin = narrowRight(begin)
      if(wideRightBegin(parent) eq null) {
        wideRightBegin(parent) = new Array[Int](refinementsFor(parent))
        Arrays.fill(wideRightBegin(parent), -1)
        narrowRightBegin(parent) = new Array[Int](refinementsFor(parent))
        Arrays.fill(narrowRightBegin(parent), length+1)
      }
      wideRightBegin(parent)(ref) = math.max(end, wideRightBegin(parent)(ref))
      narrowRightBegin(parent)(ref) = math.min(end, narrowRightBegin(parent)(ref))

      coarseNarrowLeft(end)(parent) = math.max(begin, coarseNarrowLeft(end)(parent))
      coarseWideLeft(end)(parent) = math.min(begin, coarseWideLeft(end)(parent))
      coarseWideRight(begin)(parent) = math.max(end, coarseWideRight(begin)(parent))
      coarseNarrowRight(begin)(parent) = math.min(end, coarseNarrowRight(begin)(parent))
    }

    /** right most place a left constituent with label l can start and end at position i. (start)(sym)(ref) */
    val narrowLeft: Array[Array[Array[Int]]] = Array.ofDim[Array[Int]](length+1, grammarSize)
    /** left most place a left constituent with label l can start and end at position i. (start)(sym)(ref) */
    val wideLeft = Array.ofDim[Array[Int]](length+1, grammarSize)
    /** left most place a right constituent with label l--which starts at position i--can end. (end)(sym)(ref) */
    val narrowRight = Array.ofDim[Array[Int]](length+1, grammarSize)
    /** right-most place a right constituent with label l--which starts at position i--can end. (end)(sym)(ref) */
    val wideRight = Array.ofDim[Array[Int]](length+1, grammarSize)

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
  }

  private def makeCoarseExtentArray(value: Int) = {
    val arr = Array.ofDim[Int](length + 1, grammarSize)
    for(arr1 <- arr) {
      Arrays.fill(arr1, value)
    }
    arr
  }

  // requirements: sum(a, b) >= a, \forall b that might be used.
  def sum(a:Double, b: Double):Double
  // possibly faster sum
  def sum(arr: Array[Double], length: Int):Double
  protected final def zero = Double.NegativeInfinity

  /*
  override def toString = {
    def decodeCell(i: Int, j: Int) = {
      val arr = top.scoreArray(TriangularArray.index(i, j))
      if(arr != null){
        (arr.zipWithIndex.collect { case (v, i) if !v.isInfinite => grammar.index.get(i) -> v}).mkString(", ")
      } else {
        ""
      }
    }
    val data = new TriangularArray[String](length+1, decodeCell _ )
    "ParseChart[" + data + "]"
  }
  */

}


object ParseChart {
  def apply[L](g: Index[L], refinements: Array[Int], length: Int) = viterbi(g, refinements, length)

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
    final def sum(arr: Array[Double], length: Int) = scalala.library.Numerics.logSum(arr, length)
  }
  type LogProbabilityParseChart[L] = ParseChart[L] with LogProbability

  @SerialVersionUID(1)
  trait Factory[+Chart[X]<:ParseChart[X]] extends Serializable {
    def apply[L](g: Index[L], refinements: Array[Int], length: Int):Chart[L]
  }

  // concrete factories:
  object viterbi extends Factory[ViterbiParseChart] {
    def apply[L](g: Index[L], refinements: Array[Int], length: Int) = new ParseChart(g, refinements, length) with Viterbi
  }

  object logProb extends Factory[LogProbabilityParseChart] {
    def apply[L](g: Index[L], refinements: Array[Int], length: Int) = new ParseChart(g, refinements, length) with LogProbability
  }

}

