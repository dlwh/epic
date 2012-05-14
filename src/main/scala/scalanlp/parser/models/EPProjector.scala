package scalanlp.parser.models

import scalanlp.parser.{DerivationScorer, ParseChart, ChartMarginal, TreeInstance}
import scalanlp.parser.projections.AnchoredPCFGProjector

trait EPProjector[L, W] {
  def project(inf: ParserInference[L, W],
              instance: TreeInstance[L, W],
              marginal: ChartMarginal[ParseChart.LogProbabilityParseChart, L, W]): DerivationScorer[L, W]
}

@SerialVersionUID(1)
class AnchoredRuleApproximator[L, W](pruningThreshold: Double = Double.NegativeInfinity) extends EPProjector[L, W] with Serializable {

  def project(inf: ParserInference[L, W],
              instance: TreeInstance[L, W],
              marginal: ChartMarginal[ParseChart.LogProbabilityParseChart, L, W]):DerivationScorer[L, W] = {
    val factory = new AnchoredPCFGProjector[L, W](marginal.grammar)
    factory.buildSpanScorer(marginal)
  }

}
