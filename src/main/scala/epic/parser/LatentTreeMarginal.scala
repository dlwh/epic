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

/**
 *
 * @author dlwh
 */

case class LatentTreeMarginal[L, W](anchoring: AugmentedAnchoring[L, W],
                                    tree: BinarizedTree[(L, IndexedSeq[Int])]) extends ParseMarginal[L, W] {

  private val stree = insideScores()
  outsideScores(stree)

  private val z = stree.label.inside.sum
  val logPartition = Scaling.toLogSpace(z, stree.label.iscale)

  def visitPostorder(spanVisitor: AnchoredVisitor[L], threshold: Double = Double.NegativeInfinity) = {
    // normalizer
    val rootScale = stree.label.iscale
    if (logPartition.isInfinite || logPartition.isNaN)
      sys.error(s"NaN or infinite $logPartition ${stree.label.inside.mkString(", ")}")

    stree.postorder foreach {
      case t@NullaryTree(Beliefs(label, labels, iScores, iScale, oScores, oScale), span) =>
        for( i <- 0 until  labels.length) {
          val l = labels(i)
          val iS = iScores(i)
          val oS = oScores(i)
          val ruleScore = Scaling.unscaleValue(iS / z * oS, iScale + oScale - rootScale)
          assert(!ruleScore.isNaN)
          // assert(exp(ruleScore) > 0, " " + ruleScore)
          spanVisitor.visitSpan(t.span.begin, t.span.end, label, l, ruleScore)
        }
      case t@UnaryTree(Beliefs(a, aLabels, _, _, aScores, aScale), Tree(Beliefs(c, cLabels, cScores,cScale, _,  _), _, _), chain, span) =>
        val rule = grammar.index(UnaryRule(grammar.labelIndex.get(a), grammar.labelIndex.get(c), chain))
        var pi = 0
        while(pi < aLabels.size) {
          val aRef = aLabels(pi)
          val opScore = aScores(pi)
          pi += 1
          var ci = 0
          while(ci < cLabels.size) {
            val cRef = cLabels(ci)
            val icScore = cScores(ci)
            ci += 1
            val ruleRef = anchoring.refined.ruleRefinementFromRefinements(rule, aRef, cRef)
            if(ruleRef != -1 ) {
              val rs = math.exp(anchoring.scoreUnaryRule(t.span.begin, t.span.end, rule, ruleRef)) // exp!
              val ruleScore = Scaling.unscaleValue(opScore / z * rs * icScore, aScale + cScale - rootScale)
              assert(!ruleScore.isNaN)
              // assert(exp(ruleScore) > 0, " " + ruleScore)
              spanVisitor.visitUnaryRule(t.span.begin, t.span.end, rule, ruleRef, ruleScore)
            }
          }
        }
      case t@BinaryTree(Beliefs(a, aLabels, _, _, aScores, aScale), Tree(Beliefs(b, bLabels, bScores, bScale, _, _), _, _), Tree(Beliefs(c, cLabels, cScores, cScale, _,  _), _, _), span) =>
        val begin = span.begin
        val split = t.rightChild.span.begin
        val end = t.span.end
        val rule = grammar.index(BinaryRule(grammar.labelIndex.get(a),
          grammar.labelIndex.get(b),
          grammar.labelIndex.get(c)))
        for {
          (aRef, opScore) <- aLabels zip aScores
          (bRef, ilScore) <- bLabels zip bScores
          (cRef, irScore) <- cLabels zip cScores
        } {
          val ruleRef = anchoring.refined.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
          val rs = math.exp(anchoring.scoreBinaryRule(begin, split, end, rule, ruleRef) + anchoring.scoreSpan(begin, end, a, aRef)) // exp!
          val count = Scaling.unscaleValue(opScore / z * rs * ilScore * irScore, aScale + bScale + cScale - rootScale)
          spanVisitor.visitSpan(begin, end, a, aRef, count)
          spanVisitor.visitBinaryRule(begin, split, end, rule, ruleRef, count)
        }
    }
  }


  // private stuff to do the computation

  private def insideScores() = {
    val indexedTree:BinarizedTree[Beliefs[L]] = tree.map{ case (l, refs) => Beliefs(grammar.labelIndex(l), refs) }
    val arr = new Array[Double](64 * 64)

    indexedTree.postorder.foreach {
      case t@NullaryTree(Beliefs(label, refs, scores, _, _, _), span) =>
        // fill in POS tags:
        assert(t.span.length == 1)
        var foundOne = false
        for {
           i <- 0 until scores.length
           ref = refs(i)
           wScore =  anchoring.scoreSpan(t.span.begin, t.span.end, label, ref)
           if !wScore.isInfinite
        } {
          scores(i) = math.exp(wScore) // exp!
          assert(!wScore.isInfinite)
          assert(!wScore.isNaN)
          foundOne = true
        }
        if(!foundOne) {
          sys.error(s"Trouble with lexical  $words(t.span.begin)")
        }
        t.label.scaleInside(0)
      case t@UnaryTree(Beliefs(a, aLabels, aScores, _, _, _), Tree(Beliefs(c, cLabels, cScores, cScale, _,  _), _, _), chain, span) =>
        val rule = grammar.index(UnaryRule(grammar.labelIndex.get(a), grammar.labelIndex.get(c), chain))
        var foundOne = false
        var ai = 0
        while(ai < aLabels.length) {
          val aRef = aLabels(ai)

          var sum = 0.0
          var i = 0
          var ci = 0
          while(ci < cLabels.length) {
            val cRef = cLabels(ci)
            val ruleRef = anchoring.refined.ruleRefinementFromRefinements(rule, aRef, cRef)
            if (ruleRef != -1) {
              val score = anchoring.scoreUnaryRule(t.span.begin, t.span.end, rule, ruleRef)
              val ruleScore =  cScores(ci) * math.exp(score) // exp!
              sum += ruleScore
              assert(!ruleScore.isNaN)
              if(ruleScore != 0.0) {
                foundOne = true
              }
              i += 1
            }
            ci += 1
          }

          aScores(ai) = sum
          ai += 1
        }

        if(!foundOne) {
          sys.error(s"Trouble with unary $t.render(words)}  ${grammar.labelIndex.get(a)}  ${grammar.labelIndex.get(c)}  $rule  ${anchoring.scoreUnaryRule(t.span.begin, t.span.end, rule, 0)}")
        }
        t.label.scaleInside(cScale)
      case t@BinaryTree(Beliefs(a, aLabels, aScores, _, _, _),
                        Tree(Beliefs(b, bLabels, bScores, bScale, _, _), _, _),
                        Tree(Beliefs(c, cLabels, cScores, cScale, _, _), _, _), span) =>
        val rule = grammar.index(BinaryRule(grammar.labelIndex.get(a),
          grammar.labelIndex.get(b),
          grammar.labelIndex.get(c)))
        var foundOne = false
        val begin = span.begin
        val split = t.leftChild.span.end
        val end = span.end
        var ai = 0
        while(ai < aScores.length) {
          var sum = 0.0
          val aRef = aLabels(ai)
          var bi = 0
          while(bi < bLabels.length) {
            val bRef = bLabels(bi)
            var ci = 0
            while(ci < cLabels.length) {
              val cRef = cLabels(ci)
              val ruleRef = anchoring.refined.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
              val spanScore = anchoring.scoreSpan(begin, end, a, aRef)
              sum += ( bScores(bi)
                * cScores(ci)
                * math.exp(anchoring.scoreBinaryRule(begin, split, end, rule, ruleRef) + spanScore)
                )  // exp!
              ci += 1
            }
            bi += 1
          }
          aScores(ai) = sum
          if(aScores(ai) != 0) foundOne = true
          ai += 1
        }

        if(!foundOne) {
          val r = (BinaryRule(grammar.labelIndex.get(a),
                    grammar.labelIndex.get(b),
                    grammar.labelIndex.get(c)))
          sys.error(s"Trouble with binary ${t.render(words)}\n\n$r $rule $ai")
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
      case t @ BinaryTree(_, lchild, rchild, span) =>
        val a = t.label.label
        val b = lchild.label.label
        val c = rchild.label.label
        val rule = grammar.index(BinaryRule(grammar.labelIndex.get(a),
          grammar.labelIndex.get(b),
          grammar.labelIndex.get(c)))
        for {
          (aRef, aScore) <- t.label.candidates zip t.label.outside
          bi <- 0 until lchild.label.candidates.length
          bRef = lchild.label.candidates(bi)
          bScore = lchild.label.inside(bi)
          ci <- 0 until rchild.label.candidates.length
          cRef = rchild.label.candidates(ci)
          cScore = rchild.label.inside(ci)
        } {
          val ruleRef = anchoring.refined.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
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
        val a = t.label.label
        val c = child.label.label
        val rule = grammar.index(UnaryRule(grammar.labelIndex.get(a), grammar.labelIndex.get(c), chain))
        for ( (cRef, ci) <- child.label.candidates.zipWithIndex ) {
          var sum = 0.0
          for {
            (aRef, aScore) <- t.label.candidates zip t.label.outside
          } {
            val ruleRef = anchoring.refined.ruleRefinementFromRefinements(rule, aRef, cRef)
            if(ruleRef != -1) {
              val ruleScore = anchoring.scoreUnaryRule(span.begin, span.end, rule, ruleRef)
              sum += aScore * math.exp(ruleScore) // exp!
            }
          }
          child.label.outside(ci) = sum
        }
        child.label.scaleOutside(t.label.oscale)



    }

  }

}

object LatentTreeMarginal {
  def apply[L, L2, W](anchoring: AugmentedAnchoring[L, W],
                      projections: ProjectionIndexer[L, L2],
                      tree: BinarizedTree[L]): LatentTreeMarginal[L, W] = {
    new LatentTreeMarginal(anchoring,
      tree.map { l => (l, projections.localRefinements(anchoring.grammar.labelIndex(l)).toIndexedSeq)})

  }

  def apply[L, W](grammar: AugmentedGrammar[L, W],
                  words: IndexedSeq[W],
                  tree: BinarizedTree[(L,IndexedSeq[Int])]):LatentTreeMarginal[L, W] = {
    LatentTreeMarginal(grammar.anchor(words), tree)
  }

  def apply[L, L2, W](grammar: AugmentedGrammar[L, W],
                      ref: ProjectionIndexer[L, L2],
                      words: IndexedSeq[W],
                      tree: BinarizedTree[L]):LatentTreeMarginal[L, W] = {
    apply(grammar.anchor(words), ref, tree)
  }


  case class Beliefs[L](label: Int,
                   candidates: IndexedSeq[Int],
                   inside: Array[Double],
                   var iscale: Int,
                   outside: Array[Double],
                   var oscale: Int) {
    override def toString = {
      s"Beliefs($label, $candidates, ${inside.mkString("{", ", ", " }")}, ${outside.mkString("{", ", ", "}")})"
    }

    def scaleInside(currentScale: Int) {
      iscale = Scaling.scaleArray(inside, currentScale)
    }

    def scaleOutside(currentScale: Int) {
      oscale = Scaling.scaleArray(outside, currentScale)
    }
  }

  object Beliefs {
    def apply[L](label: Int, candidates: IndexedSeq[Int]):Beliefs[L] = {
      val r = new Beliefs[L](label, candidates, new Array[Double](candidates.length), 0, new Array[Double](candidates.length), 0)
//      Arrays.fill(r.inside, Double.NegativeInfinity)
//      Arrays.fill(r.outside, Double.NegativeInfinity)
      r
    }
  }
}
