package epic.parser

import epic.framework.{Marginal, StandardExpectedCounts}
import epic.trees.Production

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

/**
 * Represents marginals over trees. Can also extract expected counts
 * @author dlwh
 */
trait ParseMarginal[L, W] extends Marginal {
  def anchoring: AugmentedAnchoring[L, W]
  def grammar:BaseGrammar[L] = anchoring.grammar
  def lexicon = anchoring.lexicon
  def logPartition: Double
  def words:IndexedSeq[W] = anchoring.words
  def length = words.length


  def expectedProductionCounts: StandardExpectedCounts[Production[L, W]] = {
    val featurizer = new ProductionFeaturizer[L, W](grammar, lexicon.knownLexicalProductions.toIndexedSeq)
    val counts = StandardExpectedCounts.zero(featurizer.index)
    expectedCounts(featurizer, counts, 1.0)
  }

  def expectedCounts[Feat](featurizer: RefinedFeaturizer[L, W, Feat]): StandardExpectedCounts[Feat] = {
    val counts = StandardExpectedCounts.zero(featurizer.index)
    expectedCounts(featurizer, counts, 1.0)
  }

  def expectedCounts[Feat](featurizer: RefinedFeaturizer[L, W, Feat], counts: StandardExpectedCounts[Feat], scale: Double): StandardExpectedCounts[Feat] = {
    val spec = featurizer.anchor(words)
    val visitor = ParseMarginal.mkVisitor(counts, spec, scale)
    visit(visitor)
    counts.loss += logPartition * scale
    counts
  }

  /**
   * Visits the forest (if applicable) in whatever order is appropriate
   * @param spanVisitor
   */

  def visit(spanVisitor: AnchoredVisitor[L]) { visitPostorder(spanVisitor) }
  /**
   * Forest traversal that visits spans in a "bottom up" order.
   * @param spanVisitor
   */
  def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double = Double.NegativeInfinity)
}

object ParseMarginal {
  private def mkVisitor[L, W, Feat](counts: StandardExpectedCounts[Feat],
                                    spec: RefinedFeaturizer[L, W, Feat]#Anchoring,
                                    scale: Double):AnchoredVisitor[L] = {
    new AnchoredVisitor[L] {
      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
        addScale(counts, spec.featuresForBinaryRule(begin, split, end, rule, ref), score * scale)
      }

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
        addScale(counts, spec.featuresForUnaryRule(begin, end, rule, ref), score * scale)
      }

      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
       // if(begin+1 == end)
         // println(begin,end,tag,ref,score)
        addScale(counts, spec.featuresForSpan(begin, end, tag, ref), score * scale)
      }
    }

  }

  private def addScale[Feat](counts: StandardExpectedCounts[Feat], features: Array[Int], score: Double) {
    val data = counts.counts.data
    var i = 0
    while(i < features.length) {
      data(features(i)) += score
      i += 1
    }
  }
}
