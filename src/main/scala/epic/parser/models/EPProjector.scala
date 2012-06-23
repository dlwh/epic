package epic.parser
package models

import epic.parser.{RefinedAnchoring, ParseChart, ChartMarginal}
import epic.parser.projections.AnchoredPCFGProjector
import epic.trees.TreeInstance

trait EPProjector[L, W] {
  def project(inf: ParserInference[L, W],
              instance: TreeInstance[L, W],
              marginal: ChartMarginal[ParseChart.LogProbabilityParseChart, L, W]): CoreAnchoring[L, W]
}

@SerialVersionUID(1)
class AnchoredRuleApproximator[L, W](pruningThreshold: Double = Double.NegativeInfinity) extends EPProjector[L, W] with Serializable {

  def project(inf: ParserInference[L, W],
              instance: TreeInstance[L, W],
              marginal: ChartMarginal[ParseChart.LogProbabilityParseChart, L, W]):CoreAnchoring[L, W] = {
    val factory = new AnchoredPCFGProjector[L, W](marginal.grammar)
    factory.project(marginal)
  }

}
