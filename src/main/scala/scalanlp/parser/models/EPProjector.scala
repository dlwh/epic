package scalanlp.parser
package models

import scalanlp.parser.{RefinedAnchoring, ParseChart, ChartMarginal}
import scalanlp.parser.projections.AnchoredPCFGProjector
import scalanlp.trees.TreeInstance

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
    factory.buildSpanScorer(marginal)
  }

}
