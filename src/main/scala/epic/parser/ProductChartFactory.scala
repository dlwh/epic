package epic.parser

import epic.util.SafeLogging
import epic.parser.projections.AnchoredRuleMarginalProjector

/**
 * TODO
 *
 * @author dlwh
 **/
class ProductChartFactory[L, W](grammars: IndexedSeq[RefinedGrammar[L, W]], maxIterations: Int = 5) extends ChartMarginal.Factory[L, W] with SafeLogging {
  def apply(words: IndexedSeq[W], initialCore: CoreAnchoring[L, W]): ChartMarginal[L, W] = {
    val anchorings = grammars.map(_ anchor words)

    if(anchorings.length == 1) {
      return ChartMarginal(AugmentedAnchoring(anchorings.head, initialCore))
    }


    val proj = new AnchoredRuleMarginalProjector[L, W]
    val augments = anchorings.map(_.marginal).map(proj.project(_))
    val marg = augments.reduceLeft[CoreAnchoring[L, W]](_ * _).marginal
    marg
  }
}
