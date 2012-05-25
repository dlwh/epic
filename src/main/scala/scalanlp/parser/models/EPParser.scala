package scalanlp.parser
package models

import scalala.tensor.dense.DenseVector
import scalanlp.epic.{EPModel, EPInference}
import scalanlp.trees.{TreeInstance, BinarizedTree}

/**
 * Parser that runs EP using the EPInference object and extracts a parse
 *
 * @author dlwh
 */
class EPParser[L, W](grammar: BaseGrammar[L],
                     lexicon: Lexicon[L, W],
                     inference: EPInference[TreeInstance[L, W], CoreAnchoring[L, W]]) extends Parser[L, W] with Serializable {
  def bestParse(s: Seq[W]) = {
    val inst = new TreeInstance[L, W]("", null, s)
    val augment = inference.getMarginals(inst, inference.baseAugment(inst))._2
    val marg = augment.marginal
    ChartDecoder(grammar, lexicon).extractBestParse(marg)
  }

}

object EPParser {

  trait Extractor[L, W] extends EPModel[TreeInstance[L, W], CoreAnchoring[L, W]] with ParserExtractable[L, W] {
    def grammar: BaseGrammar[L]

    def lexicon: Lexicon[L, W]

    def extractParser(weights: DenseVector[Double]) = {
      new EPParser(grammar, lexicon, inferenceFromWeights(weights))
    }
  }

  def fromChartParsers[L, W](grammar: BaseGrammar[L],
                             lexicon: Lexicon[L, W],
                             base: CoreGrammar[L, W],
                             grammars: (RefinedGrammar[L, W])*) = {
    val infs = grammars.map {
      p =>
        new DiscParserInference(null, {
          (a: BinarizedTree[L], b: Seq[W]) => a.map(_ -> 0)
        },
        p,
        base)
    }
    val ep = new EPInference(infs.toIndexedSeq, 5)
    new EPParser(grammar, lexicon, ep)
  }
}