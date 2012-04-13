package scalanlp.parser
package epic

import scalala.tensor.dense.DenseVector
import scalanlp.trees.BinarizedTree

/**
 * Parser that runs EP using the EPInference object and extracts a parse
 *
 * @author dlwh
 */
class EPParser[L, W](grammar: Grammar[L],
                     lexicon: Lexicon[L,  W],
                     inference: EPInference[TreeInstance[L, W], SpanScorerFactor[L, W]]) extends Parser[L, W] with Serializable {
  def bestParse(s: Seq[W]) = {
    val inst = new TreeInstance[L, W]("", null, s)
    val augment = inference.getMarginals(inst, new SpanScorerFactor(grammar, lexicon, s, UnrefinedDerivationScorer.identity))._2
    val marg = ChartMarginal.fromSentence(grammar, lexicon, augment.scorer, s)
    ChartDecoder(grammar, lexicon).extractBestParse(marg)
  }

}

object EPParser {
  trait Extractor[L, W] extends EPModel[TreeInstance[L, W], SpanScorerFactor[L, W]] with ParserExtractable[L, W] {
    def grammar: Grammar[L]
    def lexicon: Lexicon[L, W]

    def extractParser(weights: DenseVector[Double]) = {
      new EPParser(grammar, lexicon, inferenceFromWeights(weights))
    }
  }

  /*
  def fromChartParsers[L, W](grammar: Grammar[L],
                             lexicon: Lexicon[L, W],
                             grammars: (DerivationScorer.Factory[L, W])*) = {
    val infs = grammars.map{ p =>
      new DiscParserInference(null,
      {(a:BinarizedTree[L], b:Seq[W])=>a},
        p,
        null)
    }
    new EPParser(grammar, lexicon, infs)
  }
  */
}