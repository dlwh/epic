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

/**
 * Projects a chart to a span anchoring
 * @author dlwh
 */
trait ChartProjector[L, W] {
  type MyAnchoring <: UnrefinedGrammarAnchoring[L, W]
  protected def threshold:Double
  protected def createAnchoring(charts: ParseMarginal[L, W],
                                ruleData: AnchoredForestProjector.ForestData,
                                sentProb: Double):MyAnchoring

  private def proj = new AnchoredForestProjector(threshold)

  def project(charts: ParseMarginal[L, W],
              goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):MyAnchoring = {

    if (charts.logPartition.isInfinite) throw new NoParseException("infinite partition", charts.words)
    val ruleData = proj.projectRulePosteriors(charts, goldTagPolicy)
    createAnchoring(charts, ruleData, charts.logPartition)
  }
}
