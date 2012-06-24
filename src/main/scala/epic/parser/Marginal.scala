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

/**
 * Represents marginals over trees. Can also extract expected counts
 * @author dlwh
 */
trait Marginal[L, W] {
  def anchoring: AugmentedAnchoring[L, W]
  def grammar:BaseGrammar[L] = anchoring.grammar
  def lexicon = anchoring.lexicon
  def partition: Double
  def words:Seq[W] = anchoring.words
  def length = words.length

  def expectedCounts[Feat](featurizer: RefinedFeaturizer[L, W, Feat]): ExpectedCounts[Feat] = {
    val spec = featurizer.anchor(words)
    val counts = new ExpectedCounts[Feat](featurizer.index)
    val visitor = Marginal.mkVisitor(counts, spec)
    visit(visitor)
    counts.loss = partition
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

object Marginal {
  private def mkVisitor[L, W, Feat](counts: ExpectedCounts[Feat],
                                    spec: RefinedFeaturizer[L, W, Feat]#Anchoring):AnchoredVisitor[L] = {
    new AnchoredVisitor[L] {
      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
        addScale(counts, spec.featuresForBinaryRule(begin, split, end, rule, ref), score)
      }

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
        addScale(counts, spec.featuresForUnaryRule(begin, end, rule, ref), score)
      }

      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
       // if(begin+1 == end)
         // println(begin,end,tag,ref,score)
        addScale(counts, spec.featuresForSpan(begin, end, tag, ref), score)
      }
    }

  }

  private def addScale[Feat](counts: ExpectedCounts[Feat], features: Array[Int], score: Double) {
    val data = counts.counts.data
    var i = 0
    while(i < features.length) {
      data(features(i)) += score
      i += 1
    }
  }
}
