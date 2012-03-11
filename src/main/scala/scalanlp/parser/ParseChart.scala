package scalanlp.parser
/*
 Copyright 2010 David Hall

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

import scalanlp.collection.mutable.TriangularArray
import scalanlp.trees.Span

import scalanlp.util.Encoder
import scalala.tensor.Counter
import scalala.tensor.dense.DenseVectorCol
import scalanlp.parser.ParseChart.FeasibleSpan
import math._
import java.util.Arrays

@SerialVersionUID(2)
abstract class ParseChart[L](val grammar: Encoder[L], val length: Int, lexicalize: Boolean = false) extends Serializable {
  val grammarSize = grammar.index.size
  val chartSize = if(lexicalize) grammarSize * length else grammarSize

  final val top = new ChartScores()
  final val bot = new ChartScores()

  final class ChartScores private[ParseChart]() extends LabelScoreArray(length,chartSize,zero) {

    private[ParseChart] def scoreArray = score
    def labelScore(begin: Int, end: Int, label: L):Double = labelScore(begin,end,grammar.index(label))

    // lexicalization stuff
    def labelScore(begin: Int, end: Int, label: Int, headWord: Int):Double = {
      labelScore(begin,end,encodeLabelPair(label,headWord))
    }
    def encodeLabelPair(label: Int, headWord: Int) = headWord * grammarSize + label
    def decodeLabelPart(labelhead: Int) = labelhead % grammarSize
    def decodeHeadPart(labelhead: Int) = labelhead / grammarSize
    def enter(begin:Int, end: Int, parent: Int, headWord: Int, w: Double):Boolean = {
      enter(begin, end, encodeLabelPair(parent, headWord), w)
    }
    def enter(begin:Int, end: Int, parent: Int, headWord: Int, w: Array[Double], length: Int):Boolean = {
      enter(begin, end, encodeLabelPair(parent, headWord), w, length)
    }

    def enter(begin: Int, end: Int, parent: Int, w: Double):Boolean = {
      val index = TriangularArray.index(begin, end)
      var arr = score(index)
      if(arr eq null) {
        score(index) = LabelScoreArray.mkGrammarVector(chartSize,zero)
        arr = score(index)
      }
      val oldScore = arr(parent)
      val newScore = sum(oldScore, w)
      score(index)(parent) = newScore

      if(oldScore == zero) {
        updateExtents(index, parent, end, begin)
      }
      newScore > oldScore
    }

    def enter(begin: Int, end: Int, parent: Int, w: Array[Double], length: Int):Boolean = {
      val index = TriangularArray.index(begin, end)
      var arr = score(index)
      if(arr eq null) {
        score(index) = LabelScoreArray.mkGrammarVector(chartSize,zero)
        arr = score(index)
      }
      val oldScore = arr(parent)
      val newScore = sum(w,length)
      arr(parent) = sum(oldScore,newScore)

      if(newScore > oldScore) {
        updateExtents(index, parent, end, begin)
      }
      newScore > oldScore
    }

    /**
     * Bulk enter: sum w in the current semiring (with length as the size) and add it in.
     * Avoids duplicating work.
     */
    private def updateExtents(index: Int, parent: Int, end: Int, begin: Int) {
      enteredLabels(index) += parent
      narrowLeft(end)(parent) = math.max(begin, narrowLeft(end)(parent))
      wideLeft(end)(parent) = math.min(begin, wideLeft(end)(parent))
      wideRight(begin)(parent) = math.max(end, wideRight(begin)(parent))
      narrowRight(begin)(parent) = math.min(end, narrowRight(begin)(parent))
    }

    /** Can a constituent with this label start here and end before end
    def canStartHere(begin: Int, end: Int, child: Int):Boolean = {
      narrowRight(begin)(child) <= end
    }*/

    /**
     * returns all the possible split points for a span over (begin,end) with left child and right child
     */
    def feasibleSpan(begin: Int, end: Int, leftState: Int, rightState: Int): FeasibleSpan = {
      val narrowR = narrowRight(begin)(leftState)
      val narrowL = narrowLeft(end)(rightState)

      if (narrowR >= end || narrowL < narrowR) {
        FeasibleSpan.empty
      } else {
        val trueMin = narrowR max wideLeft(end)(rightState)
        val trueMax = wideRight(begin)(leftState) min narrowL
        if(trueMin > narrowL || trueMin > trueMax)  FeasibleSpan.empty
        else FeasibleSpan(trueMin,trueMax+1)
      }
    }

    def feasibleSpanX(begin: Int, end: Int, leftState:Int, rightState: Int):Long = {
      val narrowR = narrowRight(begin)(leftState)
      val narrowL = narrowLeft(end)(rightState)

      if (narrowR >= end || narrowL < narrowR) {
        0L
      } else {
        val trueX = wideLeft(end)(rightState)
        val trueMin = if(narrowR > trueX) narrowR else trueX
        val wr = wideRight(begin)(leftState)
        val trueMax = if(wr < narrowL) wr else narrowL
        if(trueMin > narrowL || trueMin > trueMax)  0L
        else ((trueMin:Long) << 32) | ((trueMax + 1):Long)
      }
    }

    def rightChildExtent(begin: Int, rightState: Int):Long = {
      val nr = narrowRight(begin)(rightState)
      val wr = wideRight(begin)(rightState)
      if (nr > length || wr < 0) 0L
      else ((nr:Long) << 32)|((wr+1):Long)
    }


    def leftChildExtent(end: Int, leftState: Int):Long = {
      val nl = narrowLeft(end)(leftState)
      val wl = wideLeft(end)(leftState)
      if (wl > length || nl < 0) 0L
      else ((wl:Long) << 32)|( (nl+1):Long)
    }


    // right most place a left constituent with label l can start and end at position i
    val narrowLeft = fastFill(length+1,chartSize,-1)
    // left most place a left constituent with label l can start and end at position i
    val wideLeft = fastFill(length+1,chartSize,length+1)
    // left most place a right constituent with label l--which starts at position i--can end.
    val narrowRight = fastFill(length+1,chartSize,length+1)
    // right-most place a right constituent with label l--which starts at position i--can end.
    val wideRight = fastFill(length+1,chartSize,-1)
  }

  def fastFill(len1: Int, len2: Int, value: Int) = {
    val arr = Array.ofDim[Int](len1,len2)
    var i = 0
    while(i < len1) {
      Arrays.fill(arr(i),value)
      i += 1
    }
    arr
  }



  // requirements: sum(a,b) >= a, \forall b that might be used.
  def sum(a:Double,b: Double):Double
  def sum(arr: Array[Double], length: Int):Double
  protected final def zero = Double.NegativeInfinity

  private val emptySpan = Span(length+1,length+1)

  override def toString = {
    def decodeCell(i: Int, j: Int) = {
      val arr = top.scoreArray(TriangularArray.index(i,j))
      if(arr != null){
        (arr.zipWithIndex.collect { case (v,i) if !v.isInfinite => grammar.index.get(i) -> v}).mkString(", ")
      } else {
        ""
      }
    }
    val data = new TriangularArray[String](length+1, decodeCell _ )
    "ParseChart[" + data + "]"
  }

}


object ParseChart {
  def apply[L](g: Grammar[L], length: Int) = viterbi(g,length)

  trait Viterbi {
    final def sum(a: Double, b: Double) = {
      math.max(a,b)
    }

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
      // scalala.library.Numerics.logSum(a,b)
      if (a == Double.NegativeInfinity) b
      else if (b == Double.NegativeInfinity) a
      else if (a < b) b + log(1+exp(a - b))
      else a + log(1+exp(b - a))
    }
    final def sum(arr: Array[Double], length: Int) = scalala.library.Numerics.logSum(arr,length)
  }
  type LogProbabilityParseChart[L] = ParseChart[L] with LogProbability


  @SerialVersionUID(1)
  trait Factory[+Chart[X]<:ParseChart[X]] extends Serializable {
    def apply[L](g: Encoder[L], length: Int, lexicalize: Boolean = false):Chart[L]
//    def computeUnaryClosure[L](g: Grammar[L]):UnaryRuleClosure
  }


  // concrete factories:
  object viterbi extends Factory[ViterbiParseChart] {
    def apply[L](g: Encoder[L], length: Int, lexicalize: Boolean) = new ParseChart(g,length,lexicalize) with Viterbi
//    def computeUnaryClosure[L](grammar: Grammar[L]):UnaryRuleClosure = {
//      import scalanlp.math.Semiring.Viterbi._
//      UnaryRuleClosure.computeClosure(grammar)
//    }
  }

  object logProb extends Factory[LogProbabilityParseChart] {
    def apply[L](g: Encoder[L], length: Int, lexicalize: Boolean) = new ParseChart(g,length, lexicalize) with LogProbability
//    def computeUnaryClosure[L](grammar: Grammar[L]):UnaryRuleClosure = {
//      import scalanlp.math.Semiring.LogSpace._
//      UnaryRuleClosure.computeClosure(grammar)
//    }
  }

  final case class FeasibleSpan(begin:Int,end:Int) extends Traversable[Int] {
    @inline def foreach[U](f: Int=>U) {
      var i = begin
      while(i < end) {
        f(i)
        i += 1
      }
    }
  }

  object FeasibleSpan {
    val empty = FeasibleSpan(0,0)
  }


}

