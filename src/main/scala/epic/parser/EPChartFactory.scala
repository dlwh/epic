package epic.parser

import epic.parser.projections.AnchoredPCFGProjector
import epic.util.SafeLogging

/**
 * TODO
 *
 * @author dlwh
 **/
case class EPChartFactory[L, W](grammars: IndexedSeq[RefinedGrammar[L, W]], maxIterations: Int = 5) extends ChartMarginal.Factory[L, W] with SafeLogging {
  def apply(words: IndexedSeq[W], initialCore: CoreAnchoring[L, W]): ChartMarginal[L, W] = {
    val anchorings = grammars.map(_ anchor words)

    if(anchorings.length == 1) {
      return ChartMarginal(AugmentedAnchoring(anchorings.head, initialCore))
    }

    val projector = new AnchoredPCFGProjector[L, W](-15)
    var iter = 0
    var marginal: ChartMarginal[L, W] = null

    def project(q: CoreAnchoring[L, W], i: Int): (CoreAnchoring[L, W], Double) = {
      val inf = anchorings(i)
      marginal = inf.marginal
      var contributionToLikelihood = marginal.logPartition
      if (contributionToLikelihood.isInfinite || contributionToLikelihood.isNaN) {
        logger.error(s"Model $i is misbehaving ($contributionToLikelihood) on iter $iter! Datum: ${q.words}" )
        throw new RuntimeException("EP is being sad!")
        /*
        marg = inf.marginal(datum)
        contributionToLikelihood = marg.logPartition
        if (contributionToLikelihood.isInfinite || contributionToLikelihood.isNaN) {
          throw new RuntimeException(s"Model $i is misbehaving ($contributionToLikelihood) on iter $iter! Datum: " + datum )
        }
        */
      }
      val newAugment = projector.project(marginal)
      newAugment -> contributionToLikelihood
    }

    val ep = new nak.inference.ExpectationPropagation(project _, 1E-5)
    var state: ep.State = null
    val iterates = ep.inference(initialCore, Array.range(0, anchorings.length), Array.fill(anchorings.length)(null))
    var converged = false
    while (!converged && iter < maxIterations && iterates.hasNext) {
      val s = iterates.next()
      if (state != null) {
        converged = (s.logPartition - state.logPartition).abs / math.max(s.logPartition, state.logPartition) < 1E-5
      }

      iter += 1
      state = s
    }

    marginal
  }
}
