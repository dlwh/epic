package scalanlp.parser

import projections.GrammarRefinements
import scalanlp.trees._
import scalala.tensor.Counter2
import scalala.library.Library

object DerivationScorerFactory {
  def oneOff[L, W](scorer: DerivationScorer[L, W]): DerivationScorer.Factory[L, W] = {
    new DerivationScorer.Factory[L, W] {
      def grammar = scorer.grammar
      def lexicon = scorer.lexicon

      def specialize(words: Seq[W]) = scorer
    }
  }

  def generative[L, W](root: L,
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]):DerivationScorer.Factory[L, W] = {
    val grammar = Grammar(root, binaryProductions.keysIterator.map(_._2) ++ unaryProductions.keysIterator.map(_._2))
    val lexicon = new SimpleLexicon[L, W](wordCounts)

    generative(grammar, lexicon, binaryProductions, unaryProductions, wordCounts)
  }

  def generative[L, W](grammar: Grammar[L], lexicon: Lexicon[L, W],
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]):DerivationScorer.Factory[L, W] = {
    val loggedB = Library.logAndNormalizeRows(binaryProductions)
    val loggedU= Library.logAndNormalizeRows(unaryProductions)

    val ref = GrammarRefinements.identity(grammar)

    val ruleScoreArray = for(r <- grammar.index.toArray) yield r match {
      case r@BinaryRule(a,_,_) => loggedB(a,r)
      case r@UnaryRule(a,_) => loggedU(a,r)
    }
    
    val spanScoreArray = grammar.labelEncoder.mkArray[Double]

    refined(grammar, lexicon, ref, ruleScoreArray, spanScoreArray, new SimpleTagScorer(wordCounts))
  }

  def refined[L, L2, W](grammar: Grammar[L], lexicon: Lexicon[L, W],
                        refinements: GrammarRefinements[L, L2],
                        refinedRuleScores: Array[Double],
                        refinedSpanScores: Array[Double],
                        tagScorer: TagScorer[L2, W]) = {

    val g = grammar
    val l = lexicon
    
    val refinedGrammar = Grammar(refinements.labels.refinementsOf(grammar.root)(0),
      refinements.labels.fineIndex,
      refinements.rules.fineIndex)

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

    new DerivationScorer.Factory[L, W] {
      def grammar = g
      def lexicon = l

      def specialize(w: Seq[W]) = new Specialization {
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
          refinements.rules.localRefinements(rule)
        }

        def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
          refinements.rules.localRefinements(rule)
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
          (refinements.rules.fineIndex(UnaryRule(refinements.labels.fineIndex.get(a2),refinements.labels.fineIndex.get(b2))))
        }

        def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
          val a = grammar.parent(r)
          val b = grammar.leftChild(r)
          val c = grammar.rightChild(r)
          val a2 = refinements.labels.globalize(a, refA)
          val b2 = refinements.labels.globalize(b, refB)
          val c2 = refinements.labels.globalize(c, refC)
          (refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
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