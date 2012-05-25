package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
case class ProjectingCoreGrammar[L, W](parser: AugmentedGrammar[L, W],
                                             projector: ChartProjector[L, W]) extends CoreGrammar[L, W] {


  def grammar = parser.grammar

  def lexicon = parser.lexicon

  def specialize(words: Seq[W]) = {
    mkSpanScorer(words)
  }

  def mkSpanScorer(s: Seq[W], goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags) = {
    val charts = ChartMarginal(parser, s, ParseChart.logProb)

    projector.buildSpanScorer(charts, goldTagPolicy)
  }

}

