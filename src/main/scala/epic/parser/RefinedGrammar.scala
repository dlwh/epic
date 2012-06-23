package epic.parser

import projections.GrammarRefinements
import epic.trees._
import breeze.linalg._
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

    generative(grammar, lexicon,  binaryProductions, unaryProductions, wordCounts)
  }

  def generative[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W],
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]): SimpleRefinedGrammar[L, L, W] = {
    val ref = GrammarRefinements.identity(grammar)
    generative(grammar, lexicon, ref, binaryProductions, unaryProductions, wordCounts)
  }

  def generative[L, L2, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W],
                           refinements: GrammarRefinements[L, L2],
                           binaryProductions: Counter2[L2, BinaryRule[L2], Double],
                           unaryProductions: Counter2[L2, UnaryRule[L2], Double],
                           wordCounts: Counter2[L2, W, Double]): SimpleRefinedGrammar[L, L2, W] = {
    val loggedB:Counter2[L2, BinaryRule[L2], Double] = logAndNormalize(binaryProductions, Axis._1)
    val loggedU:Counter2[L2, UnaryRule[L2], Double] = logAndNormalize(unaryProductions, Axis._1)

    val ruleScoreArray = for(r <- refinements.rules.fineIndex.toArray) yield r match {
      case r@BinaryRule(a, _, _) => loggedB(a, r)
      case r@UnaryRule(a, _, _) => loggedU(a, r)
    }
    
    val spanScoreArray = new Array[Double](refinements.labels.fineIndex.size)

    unanchored[L, L2, W](grammar, lexicon, refinements, ruleScoreArray, spanScoreArray, new SimpleTagScorer(wordCounts))
  }

  def unanchored[L, L2, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W],
                        refinements: GrammarRefinements[L, L2],
                        refinedRuleScores: Array[Double],
                        refinedSpanScores: Array[Double],
                        tagScorer: TagScorer[L2, W]): SimpleRefinedGrammar[L, L2, W] = {

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

    new SimpleRefinedGrammar[L, L2, W](grammar, lexicon, refinements,
      refinedGrammar, ruleScoreArray, spanScoreArray,
      parentCompatibleRefinements, childCompatibleRefinements, tagScorer)


  }

}