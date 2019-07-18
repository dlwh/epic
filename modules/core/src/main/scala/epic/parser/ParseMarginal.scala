package epic.parser


import breeze.collection.mutable.TriangularArray
import breeze.features.FeatureVector
import breeze.linalg.{Counter, axpy}
import epic.constraints.ChartConstraints
import epic.framework.{StandardExpectedCounts, VisitableMarginal}
import epic.parser.projections.AnchoredSpanProjector
import epic.trees.Rule

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
trait ParseMarginal[L, W] extends VisitableMarginal[AnchoredVisitor[L]] {
  def anchoring: GrammarAnchoring[L, W]
  def topology:RuleTopology[L] = anchoring.topology

  def lexicon = anchoring.lexicon
  def logPartition: Double
  def words:IndexedSeq[W] = anchoring.words
  def length = words.length

  def isMaxMarginal: Boolean

  def feasibleSplitPoints(begin: Int, end: Int, leftChild: Int, leftChildRef: Int, rightChild: Int, rightChildRef: Int):IndexedSeq[Int]
  def insideTopScore(begin: Int, end: Int, sym: Int, ref: Int):Double
  def insideBotScore(begin: Int, end: Int, sym: Int, ref: Int):Double

  def labelMarginals: (TriangularArray[Counter[L, Double]], TriangularArray[Counter[L, Double]])  = {
    new AnchoredSpanProjector().projectSpanPosteriors(this).decode(topology)
  }

  def expectedRuleCounts: StandardExpectedCounts[Rule[L]] = {
    val featurizer = new RuleFeaturizer[L, W](topology)
    val counts = StandardExpectedCounts.zero(featurizer.index)
    expectedCounts(featurizer, counts, 1.0)
  }

  def expectedCounts[Feat](featurizer: RefinedFeaturizer[L, W, Feat]): StandardExpectedCounts[Feat] = {
    val counts = StandardExpectedCounts.zero(featurizer.index)
    expectedCounts(featurizer, counts, 1.0)
  }

  def expectedCounts[Feat](featurizer: RefinedFeaturizer[L, W, Feat], counts: StandardExpectedCounts[Feat], scale: Double): StandardExpectedCounts[Feat] = {
    val spec = featurizer.anchor(words)
    val visitor = ParseMarginal.mkVisitorFromFeaturizer(counts, spec, scale)
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

  trait Factory[L, W] {
    def apply(w: IndexedSeq[W], constraints: ChartConstraints[L]):ParseMarginal[L, W]
  }

  object Factory {
    def apply[L, W](grammar: Grammar[L, W]):StandardChartFactory[L, W] = new StandardChartFactory(grammar)
  }

  private def mkVisitorFromFeaturizer[L, W, Feat](counts: StandardExpectedCounts[Feat],
                                    spec: RefinedFeaturizer[L, W, Feat]#Anchoring,
                                    scale: Double):AnchoredVisitor[L] = {
    new AnchoredVisitor[L] {
      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
        axpy(score * scale,  new FeatureVector(spec.featuresForBinaryRule(begin, split, end, rule, ref)), counts)
      }

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
        axpy(score * scale, new FeatureVector(spec.featuresForUnaryRule(begin, end, rule, ref)), counts)
      }

      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        axpy(score * scale,  new FeatureVector(spec.featuresForSpan(begin, end, tag, ref)), counts)
      }
    }

  }

  def maxDerivationMarginal[L, W](anch: GrammarAnchoring[L, W]):ParseMarginal[L, W] = {
    val maxM = RefinedChartMarginal(anch, maxMarginal = true)
    val parse = new ViterbiDecoder().extractMaxDerivationParse(maxM)
    TreeMarginal(anch, parse)
  }

}

@SerialVersionUID(-875432696804946554L)
case class StandardChartFactory[L, W](refinedGrammar: Grammar[L, W], maxMarginal: Boolean = false) extends ParseMarginal.Factory[L, W] {
  def apply(w: IndexedSeq[W], constraints: ChartConstraints[L]):RefinedChartMarginal[L, W] = {
      val marg = RefinedChartMarginal(refinedGrammar.anchor(w, constraints), maxMarginal = maxMarginal)
      if (!marg.logPartition.isInfinite) {
        marg
      } else {
        RefinedChartMarginal(refinedGrammar.withPermissiveLexicon.anchor(w, constraints), maxMarginal = maxMarginal)
      }
  }
}
