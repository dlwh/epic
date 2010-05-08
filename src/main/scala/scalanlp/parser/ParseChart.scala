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



import scalanlp.collection.mutable.TriangularArray
import scalala.tensor.counters.Counters.DoubleCounter
import scalanlp.trees.BinarizedTree
import scalanlp.trees.BinaryTree
import scalanlp.trees.NullaryTree
import scalanlp.trees.UnaryTree
import scalanlp.trees.Span
import scalanlp.util.IntBloomFilter

final class ParseChart[L](grammar: Grammar[L], length: Int) {
  private class BackPtr;
  private case object Term extends BackPtr;
  private case class Unary(lchild: Int) extends BackPtr;
  private case class Binary(lchild: Int, rchild: Int, split: Int) extends BackPtr;

  private val score = TriangularArray.raw(length+1, grammar.mkVector(Double.NegativeInfinity));
  private val back =  TriangularArray.raw(length+1, grammar.mkSparseArray[BackPtr]);
  // right most place a left constituent with label l can start and end at position i
  private val narrowLeft = Array.fill(length+1)(grammar.fillArray[Int](-1));
  // left most place a left constituent with label l can start and end at position i
  private val wideLeft = Array.fill(length+1)(grammar.fillArray[Int](length+1));
  // left most place a right constituent with label l--which starts at position i--can end.
  private val narrowRight = Array.fill(length+1)(grammar.fillArray[Int](length+1));
  // right-most place a right constituent with label l--which starts at position i--can end.
  private val wideRight = Array.fill(length+1)(grammar.fillArray[Int](-1));

  def enterTerm(begin: Int, end: Int, label: L, w: Double): Unit = enterTerm(begin,end,grammar.index(label),w);
  def enterTerm(begin: Int, end: Int, label: Int, w: Double): Unit = {
    enter(begin,end,label,Term,w);
  }

  def enterUnary(begin: Int, end: Int, parent: L, child: L, w: Double): Unit = {
    enterUnary(begin,end,grammar.index(parent),grammar.index(child),w);
  }

  def enterUnary(begin: Int, end: Int, parent: Int, child: Int, w: Double): Unit = {
    enter(begin,end,parent,Unary(child),w);
  }

  def enterBinary(begin: Int, split: Int, end: Int, parent: L, lchild: L, rchild: L, w: Double): Unit = {
    enterBinary(begin,split,end,grammar.index(parent),grammar.index(lchild), grammar.index(rchild), w);
  }
  def enterBinary(begin: Int, split: Int, end: Int, parent: Int, lchild: Int, rchild: Int, w: Double): Unit = {
    enter(begin,end,parent,Binary(lchild,rchild,split),w);
  }

  private def enter(begin: Int, end: Int, parent: Int, ptr: BackPtr, w: Double) = {
    score(TriangularArray.index(begin,end))(parent) = w;
    back(TriangularArray.index(begin,end))(parent) = ptr;
    narrowLeft(end)(parent) = begin max narrowLeft(end)(parent);
    wideLeft(end)(parent) = begin min wideLeft(end)(parent);
    wideRight(begin)(parent) = end max wideRight(begin)(parent);
    narrowRight(begin)(parent) = end min narrowRight(begin)(parent);
  }

  def feasibleSpan(begin: Int, end: Int, leftState: Int, rightState: Int): Span = {
    val narrowR = narrowRight(begin)(leftState);
    val narrowL = narrowLeft(end)(rightState);

    if (narrowR >= end || narrowL < narrowR) {
      emptySpan
    } else {
      val trueMin = narrowR max wideLeft(end)(rightState);
      val trueMax = wideRight(begin)(leftState) min narrowL;
      if(trueMin > narrowL || trueMin > trueMax) emptySpan
      else Span(trueMin,trueMax+1)
    }
  }

  private val emptySpan = Span(length+1,length+1)

  def enteredLabelIndexes(begin: Int, end: Int) = {
    score(TriangularArray.index(begin, end)).activeDomain.iterator
  }

  def enteredLabelScores(begin: Int, end: Int) = {
    score(TriangularArray.index(begin, end)).activeElements
  }


  def labelScore(begin: Int, end: Int, label: L): Double = labelScore(begin,end,grammar.index(label));
  def labelScore(begin: Int, end: Int, label: Int): Double = {
    score(TriangularArray.index(begin, end))(label);
  }

  def dumpChart() {
    val myScore = new TriangularArray[DoubleCounter[L]](length+1,(i:Int,j:Int)=>grammar.decode(score(TriangularArray.index(i,j))));
    val myBack = new TriangularArray[Map[L,BackPtr]](length+1,(i:Int,j:Int)=>grammar.decode(back(TriangularArray.index(i,j))));
    println(myScore);
    println(myBack);
  }


  def buildTree(start: Int, end: Int, root: Int):BinarizedTree[L] = {
    val b = back(TriangularArray.index(start, end))(root)
    b match {
      case Term =>
        NullaryTree(grammar.index.get(root))(Span(start,end));
      case Unary(child) =>
        val c = buildTree(start,end,child);
        UnaryTree(grammar.index.get(root),c)(Span(start,end));
      case Binary(lchild,rchild,split) =>
        val lc = buildTree(start,split,lchild);
        val rc = buildTree(split,end,rchild);
        BinaryTree(grammar.index.get(root),lc,rc)(Span(start,end));
    }
  }

}
