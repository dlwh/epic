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

@SerialVersionUID(2)
abstract class ParseChart[L](val grammar: Encoder[L], val length: Int) extends Serializable {
  final val top = new ChartScores()
  final val bot = new ChartScores()

  final class ChartScores private[ParseChart]() extends LabelScoreArray(length,grammar.index.size,zero) {

    private[ParseChart] def scoreArray = score
    def labelScore(begin: Int, end: Int, label: L):Double = labelScore(begin,end,grammar.index(label))

    def enter(begin: Int, end: Int, parent: Int, w: Double) = {
      val index = TriangularArray.index(begin, end)
      var arr = score(index)
      if(arr eq null) {
        score(index) = LabelScoreArray.mkGrammarVector(grammar.index.size,zero)
        arr = score(index)
      }
      val oldScore = arr(parent)
      val newScore = sum(oldScore, w)
      score(index)(parent) = newScore

      if(oldScore == zero) {
        enteredLabels(index) += parent
        narrowLeft(end)(parent) = math.max(begin, narrowLeft(end)(parent))
        wideLeft(end)(parent) = math.min(begin, wideLeft(end)(parent))
        wideRight(begin)(parent) = math.max(end,wideRight(begin)(parent))
        narrowRight(begin)(parent) = math.min(end,narrowRight(begin)(parent))
      }
      newScore > oldScore
    }

    /**
     * Bulk enter: sum w in the current semiring (with length as the size) and add it in.
     * Avoids duplicating work.
     */
    def enter(begin: Int, end: Int, parent: Int, w: Array[Double], length: Int) = {
      val index = TriangularArray.index(begin, end)
      var arr = score(index)
      if(arr eq null) {
        score(index) = LabelScoreArray.mkGrammarVector(grammar.index.size,zero)
        arr = score(index)
      }
      val oldScore = arr(parent)
      val newScore = sum(w,length)
      arr(parent) = sum(oldScore,newScore)

      if(newScore > oldScore) {
        enteredLabels(index) += parent
        narrowLeft(end)(parent) = math.max(begin, narrowLeft(end)(parent))
        wideLeft(end)(parent) = math.min(begin, wideLeft(end)(parent))
        wideRight(begin)(parent) = math.max(end,wideRight(begin)(parent))
        narrowRight(begin)(parent) = math.min(end,narrowRight(begin)(parent))
      }
      newScore > oldScore
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
    private val narrowLeft = Array.fill(length+1)(grammar.fillArray[Int](-1))
    // left most place a left constituent with label l can start and end at position i
    private val wideLeft = Array.fill(length+1)(grammar.fillArray[Int](length+1))
    // left most place a right constituent with label l--which starts at position i--can end.
    private val narrowRight = Array.fill(length+1)(grammar.fillArray[Int](length+1))
    // right-most place a right constituent with label l--which starts at position i--can end.
    private val wideRight = Array.fill(length+1)(grammar.fillArray[Int](-1))
  }



  // requirements: sum(a,b) >= a, \forall b that might be used.
  def sum(a:Double,b: Double):Double
  def sum(arr: Array[Double], length: Int):Double
  protected final def zero = Double.NegativeInfinity

  private val emptySpan = Span(length+1,length+1)

  override def toString = {
    def decodeCell(i: Int, j: Int) = {
      val arr = top.scoreArray(TriangularArray.index(i,j))
      val decoded = Seq() ++ (arr.zipWithIndex.collect { case (v,i) if !v.isInfinite => grammar.index.get(i) -> v})
      decoded.mkString(", ")
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
      if (a.isNegInfinity) b
      else if (b.isNegInfinity) a
      else if (a < b) b + log(1+exp(a - b))
      else a + log(1+exp(b - a))
    }
    final def sum(arr: Array[Double], length: Int) = scalala.library.Numerics.logSum(arr,length)
  }
  type LogProbabilityParseChart[L] = ParseChart[L] with LogProbability


  @SerialVersionUID(1)
  trait Factory[Chart[X]<:ParseChart[X]] extends Serializable {
    def apply[L](g: Encoder[L], length: Int):Chart[L]
//    def computeUnaryClosure[L](g: Grammar[L]):UnaryRuleClosure
  }


  // concrete factories:
  object viterbi extends Factory[ViterbiParseChart] {
    def apply[L](g: Encoder[L], length: Int) = new ParseChart(g,length) with Viterbi
//    def computeUnaryClosure[L](grammar: Grammar[L]):UnaryRuleClosure = {
//      import scalanlp.math.Semiring.Viterbi._
//      UnaryRuleClosure.computeClosure(grammar)
//    }
  }

  object logProb extends Factory[LogProbabilityParseChart] {
    def apply[L](g: Encoder[L], length: Int) = new ParseChart(g,length) with LogProbability
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

