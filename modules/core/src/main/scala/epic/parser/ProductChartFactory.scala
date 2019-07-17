package epic.parser

import breeze.util.SerializableLogging
import epic.parser.projections.AnchoredRuleMarginalProjector
import epic.constraints.ChartConstraints

/**
 * TODO
 *
 * @author dlwh
 **/
class ProductChartFactory[L, W](grammars: IndexedSeq[Grammar[L, W]], maxIterations: Int = 5) extends ParseMarginal.Factory[L, W] with SerializableLogging {
  def apply(words: IndexedSeq[W], initialCore: ChartConstraints[L]): RefinedChartMarginal[L, W] = {
    val anchorings = grammars.map(_.anchor(words, initialCore))

    if (anchorings.length == 1) {
      return RefinedChartMarginal(anchorings.head)
    }

    val proj = new AnchoredRuleMarginalProjector[L, W]
    val augments = anchorings.map(_.marginal).map(proj.project(_))
    val marg = augments.reduceLeft[UnrefinedGrammarAnchoring[L, W]](_ * _).marginal
    marg
  }
}
