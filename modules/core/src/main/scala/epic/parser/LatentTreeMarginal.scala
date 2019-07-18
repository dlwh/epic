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
import projections.{GrammarRefinements, ProjectionIndexer}
import java.util.Arrays
import epic.trees._
import breeze.numerics._

import LatentTreeMarginal._
import scala._
import java.util
import breeze.linalg.Counter2

/**
 *
 * @author dlwh
 */

case class LatentTreeMarginal[L, W](anchoring: GrammarAnchoring[L, W],
                                    tree: BinarizedTree[IndexedSeq[(L, Int)]]) extends ParseMarginal[L, W] {

  private val stree = insideScores()
  outsideScores(stree)

  def isMaxMarginal: Boolean = false

  private val z = stree.label.inside.sum
  val logPartition = Scaling.toLogSpace(z, stree.label.iscale)

  def visitPostorder(spanVisitor: AnchoredVisitor[L], threshold: Double = Double.NegativeInfinity) = {
    // normalizer
    val rootScale = stree.label.iscale
    if (logPartition.isInfinite || logPartition.isNaN)
      sys.error(s"NaN or infinite $logPartition ${stree.label.inside.mkString(", ")}")

    stree.postorder foreach {
      case t@NullaryTree(Beliefs(labels, iScores, iScale, oScores, oScale), span) =>
        for( i <- labels.indices) {
          val (l, ref) = labels(i)
          val iS = iScores(i)
          val oS = oScores(i)
          val ruleScore = Scaling.unscaleValue(iS / z * oS, iScale + oScale - rootScale)
          assert(!ruleScore.isNaN)
          // assert(exp(ruleScore) > 0, " " + ruleScore)
          spanVisitor.visitSpan(t.span.begin, t.span.end, l, ref, ruleScore)
        }
      case t@UnaryTree(Beliefs(aLabels, _, _, aScores, aScale), Tree(Beliefs(cLabels, cScores,cScale, _,  _), _, _), chain, span) =>
        var pi = 0
        while (pi < aLabels.size) {
          val (a, aRef) = aLabels(pi)
          val opScore = aScores(pi)
          pi += 1
          var ci = 0
          while (ci < cLabels.size) {
            val (c, cRef) = cLabels(ci)
            val icScore = cScores(ci)
            ci += 1
            val rule = topology.index(UnaryRule(topology.labelIndex.get(a), topology.labelIndex.get(c), chain))
            val ruleRef = anchoring.ruleRefinementFromRefinements(rule, aRef, cRef)
            if (ruleRef != -1 ) {
              val rs = math.exp(anchoring.scoreUnaryRule(t.span.begin, t.span.end, rule, ruleRef)) // exp!
              val ruleScore = Scaling.unscaleValue(opScore / z * rs * icScore, aScale + cScale - rootScale)
              assert(!ruleScore.isNaN)
              // assert(exp(ruleScore) > 0, " " + ruleScore)
              spanVisitor.visitUnaryRule(t.span.begin, t.span.end, rule, ruleRef, ruleScore)
            }
          }
        }
      case t@BinaryTree(Beliefs(aLabels, _, _, aScores, aScale), Tree(Beliefs(bLabels, bScores, bScale, _, _), _, _), Tree(Beliefs(cLabels, cScores, cScale, _,  _), _, _), span) =>
        val begin = span.begin
        val split = t.rightChild.span.begin
        val end = t.span.end
        for {
          ((a, aRef), opScore) <- aLabels zip aScores
          ((b, bRef), ilScore) <- bLabels zip bScores
          ((c, cRef), irScore) <- cLabels zip cScores
        } {
          val rule = topology.index(BinaryRule(topology.labelIndex.get(a),
            topology.labelIndex.get(b),
            topology.labelIndex.get(c)))
          val ruleRef = anchoring.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
          val rs = math.exp(anchoring.scoreBinaryRule(begin, split, end, rule, ruleRef) + anchoring.scoreSpan(begin, end, a, aRef)) // exp!
          val count = Scaling.unscaleValue(opScore / z * rs * ilScore * irScore, aScale + bScale + cScale - rootScale)
          spanVisitor.visitSpan(begin, end, a, aRef, count)
          spanVisitor.visitBinaryRule(begin, split, end, rule, ruleRef, count)
        }
    }
  }

  // private stuff to do the computation

  private def insideScores() = {
    val indexedTree:BinarizedTree[Beliefs[L]] = tree.map{ labels => Beliefs(labels.map { case (l, ref) => topology.labelIndex(l) -> ref})}

    indexedTree.postorder.foreach {
      case t@NullaryTree(Beliefs(labels, scores, _, _, _), span) =>
        // fill in POS tags:
        assert(t.span.length == 1)
        var foundOne = false
        for {
           i <- scores.indices
           (label, ref) = labels(i)
           wScore =  anchoring.scoreSpan(t.span.begin, t.span.end, label, ref)
           if !wScore.isInfinite
        } {
          scores(i) = math.exp(wScore) // exp!
          assert(!wScore.isInfinite)
          assert(!wScore.isNaN)
          foundOne = true
        }
        if (!foundOne) {
          sys.error(s"Trouble with lexical  $words(t.span.begin)")
        }
        t.label.scaleInside(0)
      case t@UnaryTree(Beliefs(aLabels, aScores, _, _, _), Tree(Beliefs(cLabels, cScores, cScale, _,  _), _, _), chain, span) =>
        var foundOne = false
        var ai = 0
        while (ai < aLabels.length) {
          val (a, aRef) = aLabels(ai)
          var sum = 0.0
          var ci = 0
          while (ci < cLabels.length) {
            val (c, cRef) = cLabels(ci)
            val rule = topology.index(UnaryRule(topology.labelIndex.get(a), topology.labelIndex.get(c), chain))
            if (rule != -1) {
              val ruleRef = anchoring.ruleRefinementFromRefinements(rule, aRef, cRef)
              if (ruleRef != -1) {
                val score = anchoring.scoreUnaryRule(t.span.begin, t.span.end, rule, ruleRef)
                val ruleScore =  cScores(ci) * math.exp(score) // exp!
                sum += ruleScore
                assert(!ruleScore.isNaN)
                if (score != Double.NegativeInfinity && math.exp(score) == 0.0) {
                  println("Underflow!!!")
                }
                if (ruleScore != 0.0) {
                  foundOne = true
                }
              }
            }
            ci += 1
          }
          aScores(ai) = sum
          ai += 1
        }

        if (!foundOne) {
          sys.error("unary problems")
          // sys.error(s"Trouble with unary $t.render(words)}  ${grammar.labelIndex.get(a)}  ${grammar.labelIndex.get(c)}  $rule  ${anchoring.scoreUnaryRule(t.span.begin, t.span.end, rule, 0)}")
        }
        t.label.scaleInside(cScale)
      case t@BinaryTree(Beliefs(aLabels, aScores, _, _, _),
                        Tree(Beliefs(bLabels, bScores, bScale, _, _), _, _),
                        Tree(Beliefs(cLabels, cScores, cScale, _, _), _, _), span) =>
        var foundOne = false
        val begin = span.begin
        val split = t.leftChild.span.end
        val end = span.end
        var ai = 0
        while (ai < aScores.length) {
          var sum = 0.0
          val (a, aRef) = aLabels(ai)
          var bi = 0
          while (bi < bLabels.length) {
            val (b, bRef) = bLabels(bi)
            var ci = 0
            while (ci < cLabels.length) {
              val (c, cRef) = cLabels(ci)
              val rule = topology.index(BinaryRule(topology.labelIndex.get(a),
                topology.labelIndex.get(b),
                topology.labelIndex.get(c)))
              if (rule != -1) {
                val ruleRef = anchoring.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
                if (ruleRef != -1) {
                  val spanScore = anchoring.scoreSpan(begin, end, a, aRef)
                  sum += ( bScores(bi)
                    * cScores(ci)
                    * math.exp(anchoring.scoreBinaryRule(begin, split, end, rule, ruleRef) + spanScore)
                    )  // exp!

                }
              }
              ci += 1
            }
            bi += 1
          }
          aScores(ai) = sum
          if (aScores(ai) != 0) foundOne = true
          ai += 1
        }

        if (!foundOne) {
          // val r = (BinaryRule(grammar.labelIndex.get(a),
          //   grammar.labelIndex.get(b),
          //   grammar.labelIndex.get(c)))
          //   sys.error(s"Trouble with binary ${t.render(words)}\n\n$r $rule $ai")
        }
        t.label.scaleInside(cScale + bScale)
      case _ => sys.error("bad tree!")
    }

    indexedTree
  }

  private def outsideScores(tree: BinarizedTree[Beliefs[L]]) {
    // Root gets score exp(0) == 1
    util.Arrays.fill(tree.label.outside, 1.0)

    // Set the outside score of each child
    tree.preorder.foreach {
      case t @ BinaryTree(parent, lchild, rchild, span) =>
        for {
          ((a, aRef), aScore) <- t.label.labels zip t.label.outside
          bi <- lchild.label.labels.indices
          (b, bRef) = lchild.label.labels(bi)
          bScore = lchild.label.inside(bi)
          ci <- rchild.label.labels.indices
          (c, cRef) = rchild.label.labels(ci)
          cScore = rchild.label.inside(ci)
        } {
          val rule = topology.index(BinaryRule(topology.labelIndex.get(a),
            topology.labelIndex.get(b),
            topology.labelIndex.get(c)))

          val ruleRef = anchoring.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
          val spanScore = math.exp(
            anchoring.scoreBinaryRule(span.begin, lchild.span.end, span.end, rule, ruleRef)
              + anchoring.scoreSpan(t.span.begin, t.span.end, a, aRef)
            ) // exp!
          lchild.label.outside(bi) += aScore * cScore * spanScore
          rchild.label.outside(ci) += aScore * bScore * spanScore
        }
        lchild.label.scaleOutside(t.label.oscale + rchild.label.iscale)
        rchild.label.scaleOutside(t.label.oscale + lchild.label.iscale)
      case tree: NullaryTree[IndexedSeq[Int]] => () // do nothing
      case t @ UnaryTree(_, child, chain, span) =>
        for ( ((c, cRef), ci) <- child.label.labels.zipWithIndex ) {
          var sum = 0.0
          for {
            ((a, aRef), aScore) <- t.label.labels zip t.label.outside
          } {
            val rule = topology.index(UnaryRule(topology.labelIndex.get(a), topology.labelIndex.get(c), chain))
            val ruleRef = anchoring.ruleRefinementFromRefinements(rule, aRef, cRef)
            if (ruleRef != -1) {
              val ruleScore = anchoring.scoreUnaryRule(span.begin, span.end, rule, ruleRef)
              sum += aScore * math.exp(ruleScore) // exp!
            }
          }
          child.label.outside(ci) = sum
        }
        child.label.scaleOutside(t.label.oscale)
    }

  }

  override def feasibleSplitPoints(begin: Int, end: Int, leftChild: Int, leftChildRef: Int, rightChild: Int, rightChildRef: Int): IndexedSeq[Int] = {
    tree.findSpan(begin, end) match {
      case Some(UnaryTree(a, b@BinaryTree(_, _, _, _), chain, _)) => IndexedSeq(b.splitPoint)
      case Some(b@BinaryTree(_, _, _, _)) => IndexedSeq(b.splitPoint)
      case _ => IndexedSeq.empty
    }
  }

  override def insideTopScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    stree.findSpan(begin, end) match {
      case Some(UnaryTree(a, b, chain, span)) => a.labels.indexOf(sym -> ref) match {
        case -1 =>
          Double.NegativeInfinity
        case pos =>
          Scaling.toLogSpace(a.inside(pos), a.iscale)
      }
      case _ => Double.NegativeInfinity
    }
  }

  override def insideBotScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    stree.findSpan(begin, end) match {
      case Some(UnaryTree(_, BinaryTree(a, _, _, span2), chain, span)) => a.labels.indexOf(sym -> ref) match {
        case -1 =>
          Double.NegativeInfinity
        case pos =>
          Scaling.toLogSpace(a.inside(pos), a.iscale)
      }
      case _ => Double.NegativeInfinity
    }
  }

//  override def marginalAt(begin: Int, end: Int): Counter2[L, Int, Double] = {
//   stree.findSpan(begin, end) match {
//      case None => Counter2[L, Int, Double]()
//      case Some(Tree(a, _, _)) => Counter2( (0 until a.labels.length).map {i => (grammar.labelIndex.get(a.labels(i)._1), a.labels(i)._2, a.inside) })
//    }
//  }
}

object LatentTreeMarginal {
  def apply[L, L2, W](anchoring: GrammarAnchoring[L, W],
                      projections: ProjectionIndexer[L, L2],
                      tree: BinarizedTree[L]): LatentTreeMarginal[L, W] = {
    new LatentTreeMarginal(anchoring,
      tree.map { l => projections.localRefinements(anchoring.topology.labelIndex(l)).toIndexedSeq.map(l -> _)})

  }

  def apply[L, W](grammar: Grammar[L, W],
                  words: IndexedSeq[W],
                  tree: BinarizedTree[IndexedSeq[(L, Int)]]):LatentTreeMarginal[L, W] = {
    LatentTreeMarginal(grammar.anchor(words), tree)
  }

  def apply[L, L2, W](grammar: Grammar[L, W],
                      ref: ProjectionIndexer[L, L2],
                      words: IndexedSeq[W],
                      tree: BinarizedTree[L]):LatentTreeMarginal[L, W] = {
    apply(grammar.anchor(words), ref, tree)
  }


  private case class Beliefs[L](labels: IndexedSeq[(Int, Int)],
                                inside: Array[Double],
                                var iscale: Int,
                                outside: Array[Double],
                                var oscale: Int) {
    override def toString = {
      s"Beliefs($labels, ${inside.mkString("{", ", ", " }")}, ${outside.mkString("{", ", ", "}")})"
    }

    def scaleInside(currentScale: Int) {
      iscale = Scaling.scaleArray(inside, currentScale)
    }

    def scaleOutside(currentScale: Int) {
      oscale = Scaling.scaleArray(outside, currentScale)
    }
  }

  private object Beliefs {
    private[LatentTreeMarginal] def apply[L](labels: IndexedSeq[(Int, Int)]):Beliefs[L] = {
      val r = new Beliefs[L](labels, new Array[Double](labels.length), 0, new Array[Double](labels.length), 0)
      // Arrays.fill(r.inside, Double.NegativeInfinity)
      // Arrays.fill(r.outside, Double.NegativeInfinity)
      r
    }
  }
}
