package epic.parser
package projections

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
import projections.AnchoredForestProjector.ForestData
import breeze.collection.mutable.{TriangularArray, OpenAddressHashArray}
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints

/**
 * Creates a grammar using only span marginals and unary rule marginals
 * @author dlwh
 */
case class LabeledSpanProjector[L, W](topology: RuleTopology[L], threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {

  type MyAnchoring = SpanAnchoring[L, W]
  private def normalize(ruleScores: OpenAddressHashArray[Double], totals: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if (ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity)
      for( (rule, score) <- ruleScores.activeIterator) {
        val parent = topology.parent(rule)
        if (score > 0.9999999) {
          r(rule) = 10
        } else if (score > 0) {
          r(rule) = math.log(score) - math.log1p(-score)
        }
      }
      r
    }
  }

  private def normalizeSpans(totals: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if (totals eq null) null
    else {
      val r = new OpenAddressHashArray[Double](totals.length, Double.NegativeInfinity)
      for( (parent, score) <- totals.activeIterator) {
        if (score > 0.9999999) {
          r(parent) = 10
        } else if (score > 0) {
          r(parent) = math.log(score) - math.log1p(-score)
        }
      }
      r
    }
  }

  protected def createAnchoring(charts: ParseMarginal[L, W], ruleData: ForestData, sentProb: Double) = {
    val AnchoredForestProjector.ForestData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totalsBinaries) = ruleData
    val normUnaries:Array[OpenAddressHashArray[Double]] = for((ruleScores, totals) <- unaryScores zip totalsUnaries) yield {
      normalize(ruleScores, totals)
    }

    val normSpans:Array[OpenAddressHashArray[Double]] = for( totals <- totalsBinaries) yield {
      normalizeSpans(totals)
    }

    val sparsity = charts.anchoring.sparsityPattern
    new SpanAnchoring(charts.topology, charts.lexicon, charts.words, sparsity, normSpans, normUnaries)
  }

}

/**
 * A SpanAnchoring just scores spans and unary rules. BinaryRules are all given score 0.0
 * @param topology
 * @param lexicon
 * @param words
 * @param spanScores
 * @param unaryScores
 * @tparam L
 * @tparam W
 */
@SerialVersionUID(1L)
case class SpanAnchoring[L, W](topology: RuleTopology[L],
                               lexicon: Lexicon[L, W],
                               words: IndexedSeq[W],
                               sparsityPattern: ChartConstraints[L],
                               spanScores: Array[OpenAddressHashArray[Double]],
                               unaryScores: Array[OpenAddressHashArray[Double]])  extends UnrefinedGrammarAnchoring[L, W] {
  def addConstraints(cs: ChartConstraints[L]) = copy(sparsityPattern = sparsityPattern & cs)
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if (forSpan eq null) Double.NegativeInfinity
    else forSpan(rule)
  }

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    val scores = spanScores(TriangularArray.index(begin, end))
    if (scores ne null) scores(tag)
    else Double.NegativeInfinity
  }
}

