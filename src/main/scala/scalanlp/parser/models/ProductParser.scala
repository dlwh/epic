package scalanlp.parser
package models

import scalala.tensor.dense.DenseVector
import scalanlp.trees.BinarizedTree
import projections.{AnchoredRuleMarginalProjector, ProjectingScorerFactory}

/**
 * Parser that runs EP using the EPInference object and extracts a parse
 *
 * @author dlwh
 */
class ProductParser[L, W](grammar: Grammar[L],
                          lexicon: Lexicon[L, W],
                          factories: DerivationScorer.Factory[L, W]*) extends Parser[L, W] with Serializable {
  val proj = new AnchoredRuleMarginalProjector[L, W]

  def bestParse(s: Seq[W]) = {
    val augments = factories.map(_.specialize(s).marginal).map(proj.buildSpanScorer(_))
    val marg = augments.reduceLeft[DerivationScorer[L, W]](_ * _).marginal
    ChartDecoder(grammar, lexicon).extractBestParse(marg)
  }

}

object ProductParser {
  def fromChartParsers[L, W](grammar: Grammar[L],
                             lexicon: Lexicon[L, W],
                             grammars: (DerivationScorer.Factory[L, W])*) = {
    new ProductParser(grammar, lexicon, grammars: _*)
  }
}
