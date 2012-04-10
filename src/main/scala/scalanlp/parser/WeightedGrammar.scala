package scalanlp.parser

import projections.GrammarRefinements
import scalanlp.trees._
import scalala.tensor.Counter2
import scalala.library.Library

/**
 * In contrast to [[scalanlp.parser.Grammar]], this grammar
 * is tasked with assigning weights to derivations.
 *
 * Rather than inheriting [[scalanlp.parser.Grammar]], this trait
 * has one.
 *
 * @author dlwh
 */
trait WeightedGrammar[L, W] extends Serializable {
  def grammar: Grammar[L]

  def root = grammar.root
  def index = grammar.index
  def labelIndex = grammar.labelIndex
  def labelEncoder = grammar.labelEncoder

  def specialize(words: Seq[W]):Specialization

  /**
   * A Specialization is basically a refined grammar that has been tuned to a particular sentence (if applicable).
   * It knows how to to two things: assign scores to rules and spans, and determine reachability of various refinements.
   *
   * It might be nice to consider a refined grammar that doesn't need sentence-specific tuning, but
   * that interferes with integrating lexicalization into the framework.
   */
  trait Specialization {

    def root = WeightedGrammar.this.root
    def grammar = WeightedGrammar.this

    def words: Seq[W]


    /**
     * Scores the indexed label rule with refinenemnt ref, when it occurs at (begin, end). Can be used for s, or for a
     * "bottom" label. Mainly used for s.
     */
    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int):Double

    /**
     * Scores the indexed [[scalanlp.trees.BinaryRule]] rule when it occurs at (begin, split, end)
     */
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int):Double

    /**
     * Scores the indexed [[scalanlp.trees.UnaryRule]] rule when it occurs at (begin, end)
     */
    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int):Double

    // stuff related to reachability
    /**
     * For a given span, what refinements to the label are allowed?
     * Refinements in general are in the range (0, numValidRefinements). This
     * method may return a subset.
     * @return array of valid refinements. Don't modify!
     */
    def validLabelRefinements(begin: Int, end: Int, label: Int):Array[Int]

    def numValidRefinements(label: Int):Int

    /**
     * For a given span and the parent's refinement, what refinements to the rule are allowed?
     * @param rule
     * @param begin
     * @param end
     * @return
     */
    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int):Array[Int]

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int):Array[Int]

    /**
     * Returns the (unrefined) valid s for a given position
     * @param pos
     * @return
     */
    def validTagsFor(pos: Int):Array[Int]

    def leftChildRefinement(rule: Int, ruleRef: Int):Int
    def rightChildRefinement(rule: Int, ruleRef: Int):Int
    def parentRefinement(rule: Int, ruleRef: Int):Int
    def childRefinement(rule: Int, ruleRef: Int):Int


    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int):Int
    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int):Int
  }
}

object WeightedGrammar {
  def oneOff[L, W](grammar: Grammar[L], scorer: SpanScorer[L]): WeightedGrammar[L, W] = {
    val g = grammar
    new WeightedGrammar[L, W] {
      def grammar = g

      def specialize(w: Seq[W]) = new Specialization {
        def words = w

        def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
          scorer.scoreSpan(begin, end, label)
        }

        def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
          scorer.scoreBinaryRule(begin, split, end, rule)
        }

        def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
          scorer.scoreUnaryRule(begin, end, rule)
        }

        val valid = Array(0)
        val vldl = valid
        def validLabelRefinements(begin: Int, end: Int, label: Int) = (vldl)

        def numValidRefinements(label: Int) = 1

        def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
          valid
        }

        def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
          valid
        }

        def validTagsFor(pos: Int) = {
          g.indexedTags
        }

        def leftChildRefinement(rule: Int, ruleRef: Int) = (0)

        def rightChildRefinement(rule: Int, ruleRef: Int) = (0)

        def parentRefinement(rule: Int, ruleRef: Int) = (0)

        def childRefinement(rule: Int, ruleRef: Int) = (0)

        def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
          (0)
        }

        def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
          (0)
        }


      }
    }
  }

  def generative[L, W](root: L,
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       lexicon: Lexicon[L, W]):WeightedGrammar[L, W] = {
    val grammar = Grammar(root,
      binaryProductions.keysIterator.map(_._2) ++ unaryProductions.keysIterator.map(_._2),
      lexicon.tags
    )

    generative(grammar, binaryProductions, unaryProductions, lexicon)
  }

  def generative[L, W](grammar: Grammar[L],
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       lexicon: Lexicon[L, W]):WeightedGrammar[L, W] = {
    val loggedB = Library.logAndNormalizeRows(binaryProductions)
    val loggedU= Library.logAndNormalizeRows(unaryProductions)

    val ref = GrammarRefinements.identity(grammar)

    val ruleScoreArray = for(r <- grammar.index.toArray) yield r match {
      case r@BinaryRule(a,_,_) => loggedB(a,r)
      case r@UnaryRule(a,_) => loggedU(a,r)
    }
    
    val spanScoreArray = grammar.labelEncoder.mkArray[Double]

    refined(grammar, ref, ruleScoreArray, spanScoreArray, lexicon)
  }

  def refined[L, L2, W](grammar: Grammar[L],
                        refinements: GrammarRefinements[L, L2],
                        refinedRuleScores: Array[Double],
                        refinedSpanScores: Array[Double],
                        lexicon: Lexicon[L2,W]) = {

    val g = grammar
    
    val refinedGrammar = Grammar(refinements.labels.refinementsOf(grammar.root)(0),
      refinements.labels.fineIndex,
      refinements.rules.fineIndex, tags = grammar.tags.flatMap(refinements.labels.refinementsOf _))

    val ruleScoreArray: Array[Array[Double]] = Array.tabulate(grammar.index.size){ (r: Int) =>
      val refs = refinements.rules.refinementsOf(r)
      val arr = new Array[Double](refs.length)
      for (i <- 0 until refs.length) {
        arr(i) = refinedRuleScores(refs(i))
      }
      arr
    }

    val spanScoreArray: Array[Array[Double]] = Array.tabulate(grammar.labelIndex.size){ (rr: Int) =>
      val r: Int= (rr);
      val refs = refinements.labels.refinementsOf(r)
      val arr = new Array[Double](refs.length)
      for (i <- 0 until refs.length) {
        arr(i) = refinedSpanScores(refs(i))
      }
      arr
    }

    new WeightedGrammar[L, W] {
      def grammar = g

      def specialize(w: Seq[W]) = new Specialization {
        def words = w

        def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
          val baseScore = if(begin + 1 == end) {
            val fullId = refinements.labels.globalize(label, ref)
            lexicon.wordScore(words, refinements.labels.fineIndex.get(fullId), begin)
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
          refinements.rules.localRefinements(rule)
        }

        def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
          refinements.rules.localRefinements(rule)
        }

        def validTagsFor(pos: Int) = {
          (lexicon.tagScores(words(pos)).keysIterator.map(refinements.labels.fineIndex).toArray)
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
          val a = grammar.grammar.parent(r)
          val b = grammar.grammar.child(r)
          val a2 = refinements.labels.globalize(a, refA)
          val b2 = refinements.labels.globalize(b, refB)
          (refinements.rules.fineIndex(UnaryRule(refinements.labels.fineIndex.get(a2),refinements.labels.fineIndex.get(b2))))
        }

        def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
          val a = grammar.grammar.parent(r)
          val b = grammar.grammar.leftChild(r)
          val c = grammar.grammar.rightChild(r)
          val a2 = refinements.labels.globalize(a, refA)
          val b2 = refinements.labels.globalize(b, refB)
          val c2 = refinements.labels.globalize(c, refC)
          (refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
            refinements.labels.fineIndex.get(b2),
            refinements.labels.fineIndex.get(c2)
          ))  )
        }

        def numValidRefinements(label: Int) = refinements.labels.refinementsOf(label).length
      }
    }

  }

}