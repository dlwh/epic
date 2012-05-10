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
      var arr = score(index)
      if(arr eq null) {
        score(index) = new Array[Array[Double]](grammarSize)
        score(index)(parent) = LabelScoreArray.mkGrammarVector(refinementsFor(parent), zero)
        arr = score(index)
      }
      if(arr(parent) eq null) {
        arr(parent) = LabelScoreArray.mkGrammarVector(refinementsFor(parent), zero)
      }
      val oldScore = arr(parent)(ref)
      val newScore = sum(oldScore, w)
      score(index)(parent)(ref) = newScore

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
      narrowLeft(end)(parent)(ref) = math.max(begin, narrowLeft(end)(parent)(ref))
      wideLeft(end)(parent)(ref) = math.min(begin, wideLeft(end)(parent)(ref))
      wideRight(begin)(parent)(ref) = math.max(end, wideRight(begin)(parent)(ref))
      narrowRight(begin)(parent)(ref) = math.min(end, narrowRight(begin)(parent)(ref))
    }

    def feasibleSpanX(begin: Int, end: Int, leftState: Int, leftRef: Int, rightState: Int, rightRef: Int):Long = {
      val narrowR = narrowRight(begin)(leftState)(leftRef)
      val narrowL = narrowLeft(end)(rightState)(rightRef)

      if (narrowR >= end || narrowL < narrowR) {
        0L
      } else {
        val trueX = wideLeft(end)(rightState)(rightRef)
        val trueMin = if(narrowR > trueX) narrowR else trueX
        val wr = wideRight(begin)(leftState)(leftRef)
        val trueMax = if(wr < narrowL) wr else narrowL
        if(trueMin > narrowL || trueMin > trueMax)  0L
        else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
      }
    }

    def feasibleSpan(begin: Int, end: Int, leftState: Int, leftRef: Int, rightState: Int, rightRef: Int):Range = {
      val span = feasibleSpanX(begin, end, leftState, leftRef, rightState, rightRef)
      val split = (span >> 32).toInt
      val endSplit = span.toInt // lower 32 bits
      Range(split,endSplit)
    }

    def rightChildExtent(begin: Int, rightState: Int, ref: Int):Long = {
      val nr = narrowRight(begin)(rightState)(ref)
      val wr = wideRight(begin)(rightState)(ref)
      if (nr > length || wr < 0) 0L
      else ((nr:Long) << 32)|((wr+1):Long)
    }


    def leftChildExtent(end: Int, leftState: Int, ref: Int):Long = {
      val nl = narrowLeft(end)(leftState)(ref)
      val wl = wideLeft(end)(leftState)(ref)
      if (wl > length || nl < 0) 0L
      else ((wl:Long) << 32)|( (nl+1):Long)
    }


    // right most place a left constituent with label l can start and end at position i
    val narrowLeft = makeExtentArray(-1)
    // left most place a left constituent with label l can start and end at position i
    val wideLeft = makeExtentArray(length+1)
    // left most place a right constituent with label l--which starts at position i--can end.
    val narrowRight = makeExtentArray(length+1)
    // right-most place a right constituent with label l--which starts at position i--can end.
    val wideRight = makeExtentArray(-1)
  }

  private def makeExtentArray(value: Int) = {
    val arr = Array.ofDim[Array[Int]](length + 1, grammarSize)
    for(arr1 <- arr; j <- 0 until arr1.length) {
      arr1(j) = new Array[Int](refinementsFor(j))
      Arrays.fill(arr1(j), value)
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

