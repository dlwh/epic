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

import projections.GrammarRefinements
import epic.trees._
import breeze.linalg._
import collection.mutable.ArrayBuffer
import epic.lexicon.{SimpleTagScorer, TagScorer, SimpleLexicon, Lexicon}

/**
 * TODO docs
 * @tparam L
 * @tparam W
 */
trait RefinedGrammar[L, W] extends Serializable {
  def *(refined: RefinedGrammar[L, W]) = RefinedGrammar.product(this, refined)

  def topology: RuleTopology[L]
  def lexicon: Lexicon[L, W]

  def root = topology.root
  def index = topology.index
  def labelIndex = topology.labelIndex
  def labelEncoder = topology.labelEncoder

  def anchor(words: IndexedSeq[W]):RefinedAnchoring[L, W]
}

object RefinedGrammar {
  def product[L, W](f1: RefinedGrammar[L, W], f2: RefinedGrammar[L, W]):RefinedGrammar[L, W] = new RefinedGrammar[L, W] {
    def topology = f1.topology
    def lexicon = f1.lexicon

    def anchor(words: IndexedSeq[W]) = new ProductRefinedAnchoring(f1.anchor(words), f2.anchor(words))
  }

  def identity[L, W](ruleTopology: RuleTopology[L], lexicon: Lexicon[L, W]): RefinedGrammar[L, W] = {
    val g = ruleTopology
    val l = lexicon
    new RefinedGrammar[L, W] {
      def topology = g

      def lexicon = l

      def anchor(words: IndexedSeq[W]) = {
        RefinedAnchoring.identity(topology, lexicon, words)
      }
    }
  }

  def generative[L, W](root: L,
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]):SimpleRefinedGrammar[L, L, W] = {
    val grammar = RuleTopology(root, binaryProductions.keysIterator.map(_._2) ++ unaryProductions.keysIterator.map(_._2))
    val lexicon = new SimpleLexicon[L, W](grammar.labelIndex, wordCounts)

    generative(grammar, lexicon,  binaryProductions, unaryProductions, wordCounts)
  }

  def generative[L, W](topology: RuleTopology[L], lexicon: Lexicon[L, W],
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]): SimpleRefinedGrammar[L, L, W] = {
    val ref = GrammarRefinements.identity(topology)
    generative(topology, lexicon, ref, binaryProductions, unaryProductions, wordCounts)
  }

  def generative[L, L2, W](topology: RuleTopology[L], lexicon: Lexicon[L, W],
                           refinements: GrammarRefinements[L, L2],
                           binaryProductions: Counter2[L2, BinaryRule[L2], Double],
                           unaryProductions: Counter2[L2, UnaryRule[L2], Double],
                           wordCounts: Counter2[L2, W, Double]): SimpleRefinedGrammar[L, L2, W] = {
    generative(topology, lexicon, refinements, binaryProductions, unaryProductions, new SimpleTagScorer(wordCounts))
  }

  def generative[L, L2, W](topology: RuleTopology[L], lexicon: Lexicon[L, W],
                           refinements: GrammarRefinements[L, L2],
                           binaryProductions: Counter2[L2, BinaryRule[L2], Double],
                           unaryProductions: Counter2[L2, UnaryRule[L2], Double],
                           ts: TagScorer[L2, W]): SimpleRefinedGrammar[L, L2, W] = {
    val loggedB:Counter2[L2, BinaryRule[L2], Double] = logAndNormalize(binaryProductions, Axis._1)
    val loggedU:Counter2[L2, UnaryRule[L2], Double] = logAndNormalize(unaryProductions, Axis._1)

    val ruleScoreArray = for(r <- refinements.rules.fineIndex.toArray) yield r match {
      case r@BinaryRule(a, _, _) => loggedB(a, r)
      case r@UnaryRule(a, _, _) => loggedU(a, r)
    }

    unanchored[L, L2, W](topology, lexicon, refinements, ruleScoreArray, ts)
  }

  def unanchored[L, L2, W](topology: RuleTopology[L], lexicon: Lexicon[L, W],
                        refinements: GrammarRefinements[L, L2],
                        refinedRuleScores: Array[Double],
                        tagScorer: TagScorer[L2, W]): SimpleRefinedGrammar[L, L2, W] = {

    val refinedGrammar = RuleTopology(refinements.labels.refinementsOf(topology.root)(0),
      refinements.labels.fineIndex,
      refinements.rules.fineIndex)

    new SimpleRefinedGrammar[L, L2, W](topology, lexicon, refinements,
      refinedGrammar, refinedRuleScores,
      tagScorer)
  }

}