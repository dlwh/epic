package scalanlp.parser

import scalanlp.trees.BinarizedTree

@SerialVersionUID(1)
trait ChartParser[L, W] extends Parser[L, W] with Serializable {
  def charts(w: Seq[W]):ChartMarginal[ParseChart, L, W]

  def decoder: ChartDecoder[L, W]

  override def bestParse(w: Seq[W]):BinarizedTree[L] = try {
    val chart = charts(w)
    decoder.extractBestParse(chart)
  } catch {
    case e => throw e
  }
}

/**
 * A SimpleChartParser produces trees with labels C from a ChartBuilder with labels L, a decoder from C to L, and
 * projections from C to L
 * @author dlwh
 */
@SerialVersionUID(1)
class SimpleChartParser[L, W](val augmentedGrammar: AugmentedGrammar[L, W],
                              val decoder: ChartDecoder[L, W]) extends ChartParser[L, W] with Serializable {

  def charts(w: Seq[W]) = try {
    ChartMarginal(augmentedGrammar, w, ParseChart.logProb)
  } catch {
    case e =>
      try {
        ChartMarginal(AugmentedGrammar.fromRefined(augmentedGrammar.refined), w, ParseChart.logProb)
      } catch {
        case e2 =>
          throw e

      }
  }
  def grammar = augmentedGrammar.grammar

}

object SimpleChartParser {
  def apply[L, W](grammar: AugmentedGrammar[L, W]) = {
    new SimpleChartParser[L, W](grammar, new MaxRuleProductDecoder(grammar.grammar, grammar.lexicon))
  }

}
