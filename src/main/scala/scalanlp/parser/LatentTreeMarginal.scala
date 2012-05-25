package scalanlp.parser

import projections.{GrammarRefinements, ProjectionIndexer}
import java.util.Arrays
import scalanlp.trees._
import scalala.library.Numerics
import scala.math.exp

import LatentTreeMarginal._
import scala._

/**
 *
 * @author dlwh
 */

case class LatentTreeMarginal[L, W](scorer: AugmentedAnchoring[L, W],
                                    tree: BinarizedTree[(L, Seq[Int])]) extends Marginal[L, W] {

  private val stree = insideScores()
  outsideScores(stree)

  val partition = Numerics.logSum(stree.label.inside, stree.label.inside.length)

  def visitPostorder(spanVisitor: AnchoredVisitor[L]) = {
    // normalizer
    if (partition.isInfinite || partition.isNaN)
      sys.error("NAn or infinite" + partition + " " + tree.render(words))

    stree.postorder foreach {
      case t@NullaryTree(Beliefs(label, labels, iScores, oScores)) =>
        for( i <- 0 until  labels.length) {
          val l = labels(i)
          val iS = iScores(i)
          val oS = oScores(i)
          val ruleScore = (iS + oS - partition)
          assert(!ruleScore.isNaN)
          // assert(exp(ruleScore) > 0, " " + ruleScore)
          spanVisitor.visitSpan(t.span.start, t.span.end, label, l, exp(ruleScore))
        }
      case t@UnaryTree(Beliefs(a, aLabels, _, aScores), Tree(Beliefs(c, cLabels, cScores, _), _)) =>
        val rule = grammar.index(UnaryRule(grammar.labelIndex.get(a), grammar.labelIndex.get(c)))
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
            val ruleRef = scorer.refined.ruleRefinementFromRefinements(rule, aRef, cRef)
            val ruleScore = opScore + icScore + scorer.scoreUnaryRule(t.span.start, t.span.end, rule, ruleRef) - partition
            assert(!ruleScore.isNaN)
            // assert(exp(ruleScore) > 0, " " + ruleScore)
            spanVisitor.visitUnaryRule(t.span.start, t.span.end, rule, ruleRef, exp(ruleScore))
          }
        }
      case t@BinaryTree(Beliefs(a, aLabels, _, aScores), Tree(Beliefs(b, bLabels, bScores, _), _), Tree(Beliefs(c, cLabels, cScores, _), _)) =>
        val begin = t.span.start
        val split = t.rightChild.span.start
        val end = t.span.end
        val rule = grammar.index(BinaryRule(grammar.labelIndex.get(a),
          grammar.labelIndex.get(b),
          grammar.labelIndex.get(c)))
        for {
          (aRef, opScore) <- aLabels zip aScores
          (bRef, ilScore) <- bLabels zip bScores
          (cRef, irScore) <- cLabels zip cScores
        } {
          val ruleRef = scorer.refined.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
          val rs = scorer.scoreBinaryRule(begin, split, end, rule, ruleRef) + scorer.scoreSpan(begin, end, a, aRef)
          val ruleScore = opScore + irScore + ilScore + rs - partition
          val count = exp(ruleScore)
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
      case t@NullaryTree(Beliefs(label, refs, scores, _)) =>
        // fill in POS tags:
        assert(t.span.length == 1)
        val word = words(t.span.start)
        var foundOne = false
        for {
           i <- 0 until scores.length
           ref = refs(i)
           wScore =  scorer.scoreSpan(t.span.start, t.span.end, label, ref)
           if !wScore.isInfinite
        } {
          scores(i) = wScore
          assert(!wScore.isInfinite)
          foundOne = true
        }
        if(!foundOne) {
          sys.error("Trouble with lexical " + words(t.span.start))
        }
      case t@UnaryTree(Beliefs(a, aLabels, aScores, _), Tree(Beliefs(c, cLabels, cScores, _), _)) =>
        val rule = grammar.index(UnaryRule(grammar.labelIndex.get(a), grammar.labelIndex.get(c)))
        var foundOne = false
        var ai = 0
        while(ai < aLabels.length) {
          var i = 0
          while(i < cLabels.length) {
            val aRef = aLabels(ai)
            val cRef = cLabels(i)
            val ruleRef = scorer.refined.ruleRefinementFromRefinements(rule, aRef, cRef)
            val score = scorer.scoreUnaryRule(t.span.start, t.span.end, rule, ruleRef)
            val ruleScore = ( cScores(i) + score)
            if(!ruleScore.isInfinite) {
              foundOne = true
            }
            arr(i) = ruleScore
            i += 1
          }
          aScores(ai) = Numerics.logSum(arr, i)
          ai += 1
        }

        if(!foundOne) {
          sys.error("Trouble with unary " + t.render(words) + " " + grammar.labelIndex.get(a) + " "+  grammar.labelIndex.get(c) + " " + rule + " " + scorer.scoreUnaryRule(t.span.start, t.span.end, rule, 0))
        }
      case t@BinaryTree(Beliefs(a, aLabels, aScores, _),
                        Tree(Beliefs(b, bLabels, bScores, _), _),
                        Tree(Beliefs(c, cLabels, cScores, _), _)) =>
        val rule = grammar.index(BinaryRule(grammar.labelIndex.get(a),
          grammar.labelIndex.get(b),
          grammar.labelIndex.get(c)))
        var foundOne = false
        val begin = t.span.start
        val split = t.leftChild.span.end
        val end = t.span.end
        var ai = 0
        while(ai < aScores.length) {
          val aRef = aLabels(ai)
          var i = 0
          var bi = 0
          while(bi < bLabels.length) {
            val bRef = bLabels(bi)
            var ci = 0
            while(ci < cLabels.length) {
              val cRef = cLabels(ci)
              val ruleRef = scorer.refined.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
              val spanScore = scorer.scoreSpan(begin, end, a, aRef)
              arr(i) = ( bScores(bi)
                + cScores(ci)
                + scorer.scoreBinaryRule(begin, split, end, rule, ruleRef)
                + spanScore
                )
              i += 1
              ci += 1
            }
            bi += 1
          }
          aScores(ai) = Numerics.logSum(arr, i)
          if(!aScores(ai).isInfinite) foundOne = true
          ai += 1
        }

        if(!foundOne) {
          val r = (BinaryRule(grammar.labelIndex.get(a),
                    grammar.labelIndex.get(b),
                    grammar.labelIndex.get(c)))
          sys.error("Trouble with binary " + t.render(words) + "\n\n" + r + " " + rule + " " + ai)
        }
      case _ => sys.error("bad tree!")
    }

    indexedTree
  }

  private def outsideScores(tree: BinarizedTree[Beliefs[L]]) {
    // Root gets score 0
    Arrays.fill(tree.label.outside, 0.0)

    // Set the outside score of each child
    tree.preorder.foreach {
      case t @ BinaryTree(_, lchild, rchild) =>
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
          val ruleRef = scorer.refined.ruleRefinementFromRefinements(rule, aRef, bRef, cRef)
          val spanScore = (
            scorer.scoreBinaryRule(t.span.start, lchild.span.end, t.span.end, rule, ruleRef)
              + scorer.scoreSpan(t.span.start, t.span.end, a, aRef)
            )
          lchild.label.outside(bi) = Numerics.logSum(lchild.label.outside(bi), aScore + cScore + spanScore)
          rchild.label.outside(ci) = Numerics.logSum(rchild.label.outside(ci), aScore + bScore + spanScore)
        }
      case tree: NullaryTree[Seq[Int]] => () // do nothing
      case t @ UnaryTree(_, child) =>
        val a = t.label.label
        val c = child.label.label
        val rule = grammar.index(UnaryRule(grammar.labelIndex.get(a), grammar.labelIndex.get(c)))
        val arr = new Array[Double](t.label.candidates.size)
        for {
          (cRef, ci) <- child.label.candidates.zipWithIndex
        } {
          var i = 0
          for {
            (aRef, aScore) <- t.label.candidates zip t.label.outside
          } {
            val ruleRef = scorer.refined.ruleRefinementFromRefinements(rule, aRef, cRef)
            val ruleScore = scorer.scoreUnaryRule(t.span.start, t.span.end, rule, ruleRef)
            arr(i) = aScore + ruleScore
            i += 1
          }
          child.label.outside(ci) = Numerics.logSum(arr, arr.length)
        }


    }

  }

}

object LatentTreeMarginal {
  def apply[L, L2, W](scorer: AugmentedAnchoring[L, W], ref: ProjectionIndexer[L, L2], tree: BinarizedTree[L]): LatentTreeMarginal[L, W] = {
    new LatentTreeMarginal(scorer,
      tree.map { l => (l, ref.localRefinements(scorer.grammar.labelIndex(l)).toIndexedSeq)})

  }

  def apply[L, W](grammar: AugmentedGrammar[L, W],
                  words: Seq[W],
                  tree: BinarizedTree[(L,Seq[Int])]):LatentTreeMarginal[L, W] = {
    LatentTreeMarginal(grammar.specialize(words), tree)
  }

  def apply[L, L2, W](grammar: AugmentedGrammar[L, W],
                      ref: ProjectionIndexer[L, L2],
                      words: Seq[W],
                      tree: BinarizedTree[L]):LatentTreeMarginal[L, W] = {
    apply(grammar.specialize(words), ref, tree)
  }


  class Beliefs[L](val label: Int,
                           val candidates: Seq[Int],
                           val inside: Array[Double],
                           val outside: Array[Double]) {
    override def toString = {
      "Beliefs(" + label +" "  + candidates + ", " + inside.mkString("{", ", ", " }") +", " + outside.mkString("{", ", ", "}")+")"
    }

  }

  object Beliefs {
    def unapply[L](b: Beliefs[L]) = Some((b.label, b.candidates, b.inside, b.outside))

    def apply[L](label: Int, candidates: Seq[Int]):Beliefs[L] = {
      val r = new Beliefs[L](label, candidates, new Array[Double](candidates.length), new Array[Double](candidates.length))
      Arrays.fill(r.inside, Double.NegativeInfinity)
      Arrays.fill(r.outside, Double.NegativeInfinity)
      r
    }
  }
}
