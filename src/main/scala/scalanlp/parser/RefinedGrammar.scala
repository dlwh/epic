package scalanlp.parser

import projections.GrammarRefinements
import scalanlp.trees._
import scalala.tensor.Counter2
import scalala.library.Library
import collection.mutable.ArrayBuffer

/**
 * TODO docs
 * @tparam L
 * @tparam W
 */
trait RefinedGrammar[L, W] extends Serializable {
  def *(refined: RefinedGrammar[L, W]) = RefinedGrammar.product(this, refined)

  def grammar: BaseGrammar[L]
  def lexicon: Lexicon[L, W]

  def root = grammar.root
  def index = grammar.index
  def labelIndex = grammar.labelIndex
  def labelEncoder = grammar.labelEncoder

  def anchor(words: Seq[W]):RefinedAnchoring[L, W]
}

object RefinedGrammar {
  def product[L, W](f1: RefinedGrammar[L, W], f2: RefinedGrammar[L, W]):RefinedGrammar[L, W] = new RefinedGrammar[L, W] {
    def grammar = f1.grammar
    def lexicon = f1.lexicon

    def anchor(words: Seq[W]) = new ProductRefinedAnchoring(f1.anchor(words), f2.anchor(words))
  }

  def identity[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]): RefinedGrammar[L, W] = {
    val g = grammar
    val l = lexicon
    new RefinedGrammar[L, W] {
      def grammar = g

      def lexicon = l

      def anchor(words: Seq[W]) = {
        RefinedAnchoring.identity(grammar, lexicon, words)
      }
    }
  }

  def generative[L, W](root: L,
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]):RefinedGrammar[L, W] = {
    val grammar = BaseGrammar(root, binaryProductions.keysIterator.map(_._2) ++ unaryProductions.keysIterator.map(_._2))
    val lexicon = new SimpleLexicon[L, W](wordCounts)

    generative(grammar, lexicon, binaryProductions, unaryProductions, wordCounts)
  }

  def generative[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W],
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]):RefinedGrammar[L, W] = {
    val loggedB = Library.logAndNormalizeRows(binaryProductions)
    val loggedU= Library.logAndNormalizeRows(unaryProductions)

    val ref = GrammarRefinements.identity(grammar)

    val ruleScoreArray = for(r <- grammar.index.toArray) yield r match {
      case r@BinaryRule(a,_,_) => loggedB(a,r)
      case r@UnaryRule(a,_) => loggedU(a,r)
    }
    
    val spanScoreArray = grammar.labelEncoder.mkArray[Double]

    unanchored(grammar, lexicon, ref, ruleScoreArray, spanScoreArray, new SimpleTagScorer(wordCounts))
  }

  def unanchored[L, L2, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W],
                        refinements: GrammarRefinements[L, L2],
                        refinedRuleScores: Array[Double],
                        refinedSpanScores: Array[Double],
                        tagScorer: TagScorer[L2, W]) = {

    val g = grammar
    val l = lexicon
    
    val refinedGrammar = BaseGrammar(refinements.labels.refinementsOf(grammar.root)(0),
      refinements.labels.fineIndex,
      refinements.rules.fineIndex)

    // localized rule scores
    val ruleScoreArray: Array[Array[Double]] = Array.tabulate(grammar.index.size){ (r: Int) =>
      val refs = refinements.rules.refinementsOf(r)
      val arr = new Array[Double](refs.length)
      for (i <- 0 until refs.length) {
        arr(i) = refinedRuleScores(refs(i))
      }
      arr
    }

    val spanScoreArray: Array[Array[Double]] = Array.tabulate(grammar.labelIndex.size){ (r: Int) =>
      val refs = refinements.labels.refinementsOf(r)
      val arr = new Array[Double](refs.length)
      for (i <- 0 until refs.length) {
        arr(i) = refinedSpanScores(refs(i))
      }
      arr
    }

    // rule -> parentRef -> [ruleRef]
    val parentCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
      val parent = grammar.parent(r)
      val parentRefs = Array.fill(refinements.labels.refinementsOf(parent).length){ArrayBuffer[Int]()}
      for(ruleRef <- refinements.rules.refinementsOf(r)) {
        val refParent = refinements.labels.localize(refinedGrammar.parent(ruleRef))
        parentRefs(refParent) += refinements.rules.localize(ruleRef)
      }
      parentRefs.map(_.toArray)
    }

    // rule -> parentRef -> [ruleRef]
    val childCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
      if(grammar.index.get(r).isInstanceOf[UnaryRule[L]]) {
        val child = grammar.child(r)
        val childRefs = Array.fill(refinements.labels.refinementsOf(child).length){ArrayBuffer[Int]()}
        for(ruleRef <- refinements.rules.refinementsOf(r)) {
          val refChild = refinements.labels.localize(refinedGrammar.child(ruleRef))
          childRefs(refChild) += refinements.rules.localize(ruleRef)
        }
        childRefs.map(_.toArray)
      } else {
        null
      }
    }

    new RefinedGrammar[L, W] {
      def grammar = g
      def lexicon = l

      def anchor(w: Seq[W]) = new RefinedAnchoring[L, W] {
        override def toString() = "RefinedAnchoring(...)"
        val grammar = g
        val lexicon = l
        def words = w

        def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
          val baseScore = if(begin + 1 == end) {
            val fullId = refinements.labels.globalize(label, ref)
            tagScorer.scoreTag(refinements.labels.fineIndex.get(fullId), words, begin)
          } else {
            0.0
          }
          baseScore + spanScoreArray(label)(ref)
        }

        def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
          ruleScoreArray(rule)(ref)
        }

        def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
          ruleScoreArray(rule)(ref)
        }

        def validLabelRefinements(begin: Int, end: Int, label: Int) = {
          refinements.labels.localRefinements(label)
        }

        def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
          parentCompatibleRefinements(rule)(parentRef)
        }

        def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
          childCompatibleRefinements(rule)(childRef)
        }

        def leftChildRefinement(rule: Int, ruleRef: Int) = {
          val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
          refinements.labels.localize(refinedGrammar.leftChild(refinedRuleId))
        }

        def rightChildRefinement(rule: Int, ruleRef: Int) = {
          val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
          refinements.labels.localize(refinedGrammar.rightChild(refinedRuleId))
        }

        def parentRefinement(rule: Int, ruleRef: Int) = {
          val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
          refinements.labels.localize(refinedGrammar.parent(refinedRuleId))
        }

        def childRefinement(rule: Int, ruleRef: Int) = {
          val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
          refinements.labels.localize(refinedGrammar.child(refinedRuleId))
        }

        // TODO: make this not terminally slow!
        def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
          val a = grammar.parent(r)
          val b = grammar.child(r)
          val a2 = refinements.labels.globalize(a, refA)
          val b2 = refinements.labels.globalize(b, refB)
          val refinedRuleIndex = refinements.rules.fineIndex(UnaryRule(refinements.labels.fineIndex.get(a2), refinements.labels.fineIndex.get(b2)))
          if(refinedRuleIndex < 0) {
            -1
          } else {
            refinements.rules.localize(refinedRuleIndex)
          }
        }

        def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
          val a = grammar.parent(r)
          val b = grammar.leftChild(r)
          val c = grammar.rightChild(r)
          val a2 = refinements.labels.globalize(a, refA)
          val b2 = refinements.labels.globalize(b, refB)
          val c2 = refinements.labels.globalize(c, refC)
          refinements.rules.localize(refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
            refinements.labels.fineIndex.get(b2),
            refinements.labels.fineIndex.get(c2)
          ))  )
        }

        def numValidRefinements(label: Int) = refinements.labels.refinementsOf(label).length
        def numValidRuleRefinements(rule: Int) = refinements.rules.refinementsOf(rule).length
      }
    }

  }

}