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
import epic.constraints.ChartConstraints

/**
 * TODO docs
 * @tparam L
 * @tparam W
 */
trait Grammar[L, W] extends Serializable {
  def *(refined: Grammar[L, W]) = Grammar.product(this, refined)

  def topology: RuleTopology[L]
  def lexicon: Lexicon[L, W]

  def root = topology.root
  def index = topology.index
  def labelIndex = topology.labelIndex
  def labelEncoder = topology.labelEncoder

  def withPermissiveLexicon:Grammar[L, W]

  def anchor(words: IndexedSeq[W], constraints: ChartConstraints[L] = ChartConstraints.noSparsity[L]):GrammarAnchoring[L, W]
}

object Grammar {
  def product[L, W](f1: Grammar[L, W], f2: Grammar[L, W]):Grammar[L, W] = new Grammar[L, W] {
    def topology = f1.topology
    def lexicon = f1.lexicon

    override def withPermissiveLexicon: Grammar[L, W] = product(f1.withPermissiveLexicon, f2.withPermissiveLexicon)

    def anchor(words: IndexedSeq[W],
               constraints: ChartConstraints[L] = ChartConstraints.noSparsity[L]) = new ProductGrammarAnchoring(f1.anchor(words, constraints), f2.anchor(words, constraints))
  }

  def identity[L, W](ruleTopology: RuleTopology[L], lexicon: Lexicon[L, W]): Grammar[L, W] = {
    val g = ruleTopology
    val l = lexicon
    new Grammar[L, W] {
      def topology = g

      def lexicon = l

      override def withPermissiveLexicon: Grammar[L, W] = identity(ruleTopology, l.morePermissive)

      override def anchor(words: IndexedSeq[W], constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = {
        GrammarAnchoring.identity(topology, lexicon, constraints, words)
      }
    }
  }

  def generative[L, W](root: L,
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]):SimpleGrammar[L, L, W] = {
    val grammar = RuleTopology(root, binaryProductions.keysIterator.map(_._2) ++ unaryProductions.keysIterator.map(_._2))
    val lexicon = new SimpleLexicon[L, W](grammar.labelIndex, wordCounts)

    generative(grammar, lexicon,  binaryProductions, unaryProductions, wordCounts)
  }

  def generative[L, W](topology: RuleTopology[L], lexicon: Lexicon[L, W],
                       binaryProductions: Counter2[L, BinaryRule[L], Double],
                       unaryProductions: Counter2[L, UnaryRule[L], Double],
                       wordCounts: Counter2[L, W, Double]): SimpleGrammar[L, L, W] = {
    val ref = GrammarRefinements.identity(topology)
    generative(topology, lexicon, ref, binaryProductions, unaryProductions, wordCounts)
  }

  def generative[L, L2, W](topology: RuleTopology[L], lexicon: Lexicon[L, W],
                           refinements: GrammarRefinements[L, L2],
                           binaryProductions: Counter2[L2, BinaryRule[L2], Double],
                           unaryProductions: Counter2[L2, UnaryRule[L2], Double],
                           wordCounts: Counter2[L2, W, Double]): SimpleGrammar[L, L2, W] = {
    generative(topology, lexicon, refinements, binaryProductions, unaryProductions, new SimpleTagScorer(wordCounts))
  }

  def generative[L, L2, W](topology: RuleTopology[L], lexicon: Lexicon[L, W],
                           refinements: GrammarRefinements[L, L2],
                           binaryProductions: Counter2[L2, BinaryRule[L2], Double],
                           unaryProductions: Counter2[L2, UnaryRule[L2], Double],
                           ts: TagScorer[L2, W]): SimpleGrammar[L, L2, W] = {
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
                        tagScorer: TagScorer[L2, W]): SimpleGrammar[L, L2, W] = {

    val refinedGrammar = RuleTopology(refinements.labels.refinementsOf(topology.root)(0),
      refinements.labels.fineIndex,
      refinements.rules.fineIndex)

    new SimpleGrammar[L, L2, W](topology, lexicon, refinements,
      refinedGrammar, refinedRuleScores,
      tagScorer)
  }

}