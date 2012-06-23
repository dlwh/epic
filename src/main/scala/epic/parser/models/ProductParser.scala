package epic.parser
package models

import projections.AnchoredRuleMarginalProjector

/**
 * TODO
 *
 * @author dlwh
 */
class ProductParser[L, W](grammar: BaseGrammar[L],
                          lexicon: Lexicon[L, W],
                          factories: RefinedGrammar[L, W]*) extends Parser[L, W] with Serializable {
  val proj = new AnchoredRuleMarginalProjector[L, W]

  def bestParse(s: Seq[W]) = {
    val augments = factories.map(_.anchor(s).marginal).map(proj.project(_))
    val marg = augments.reduceLeft[CoreAnchoring[L, W]](_ * _).marginal
    ChartDecoder(grammar, lexicon).extractBestParse(marg)
  }

}

object ProductParser {
  def fromChartParsers[L, W](grammar: BaseGrammar[L],
                             lexicon: Lexicon[L, W],
                             grammars: (RefinedGrammar[L, W])*) = {
    new ProductParser(grammar, lexicon, grammars: _*)
  }
}
