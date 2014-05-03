package epic.parser

import epic.util.SafeLogging
import epic.parser.projections.AnchoredRuleMarginalProjector
import epic.constraints.ChartConstraints

/**
 * TODO
 *
 * @author dlwh
 **/
class ProductChartFactory[L, W](grammars: IndexedSeq[RefinedGrammar[L, W]], maxIterations: Int = 5) extends RefinedChartMarginal.Factory[L, W] with SafeLogging {
  def apply(words: IndexedSeq[W], initialCore: ChartConstraints[L]): RefinedChartMarginal[L, W] = {
    val anchorings = grammars.map(_.anchor(words, initialCore))

    if(anchorings.length == 1) {
      return RefinedChartMarginal(anchorings.head)
    }


    val proj = new AnchoredRuleMarginalProjector[L, W]
    val augments = anchorings.map(_.marginal).map(proj.project(_))
    val marg = augments.reduceLeft[CoreAnchoring[L, W]](_ * _).marginal
    marg
  }
}
