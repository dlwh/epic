package epic.parser

import epic.parser.projections.AnchoredPCFGProjector
import breeze.util.SerializableLogging
import epic.constraints.ChartConstraints
import epic.framework.{EPScorer, EPInference}
import epic.trees.TreeInstance
import epic.lexicon.Lexicon
import epic.parser.models.AnnotatedParserInference

/**
 * TODO
 *
 * @author dlwh
 **/
case class EPChartFactory[L, W](topology: RuleTopology[L], lexicon: Lexicon[L, W], epInference: EPInference[TreeInstance[L, W], UnrefinedGrammarAnchoring[L, W]]) extends ParseMarginal.Factory[L, W] with SerializableLogging {
  def apply(words: IndexedSeq[W], initialCore: ChartConstraints[L]): ParseMarginal[L, W] = {
    val scorer = epInference.scorer(TreeInstance("", null, words))
    val marg = epInference.marginal(scorer, TreeInstance("", null, words), UnrefinedGrammarAnchoring.identity(topology, lexicon, words, initialCore) )
    marg.q.marginal
  }
}

object EPChartFactory {
  def apply[L, W](grammars: Grammar[L, W]*) = {
    val infs = grammars.map(new AnnotatedParserInference(null, null, _, ChartConstraints.Factory.noSparsity))
    new EPChartFactory(grammars.head.topology, grammars.head.lexicon, new EPInference(infs.toIndexedSeq, 5))
  }

}
