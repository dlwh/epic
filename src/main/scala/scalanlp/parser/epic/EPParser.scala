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
                     inference: EPInference[TreeInstance[L, W], SpanScorerFactor[L, W]]) extends Parser[L, W] with Serializable {
  def bestParse(s: Seq[W]) = {
    val inst = new TreeInstance[L, W]("", null, s)
    val augment = inference.getMarginals(inst, new SpanScorerFactor(grammar, s, SpanScorer.identity))._2
    val wgrammar = DerivationScorerFactory.oneOff[L, W](grammar, augment.scorer)
    SimpleChartParser(wgrammar).bestParse(s)
  }

}

object EPParser {
  trait Extractor[L, W] extends EPModel[TreeInstance[L, W], SpanScorerFactor[L, W]] with ParserExtractable[L, W] {
    def grammar: Grammar[L]

    def extractParser(weights: DenseVector[Double]) = {
      new EPParser(grammar, inferenceFromWeights(weights))
    }
  }

  /*
  def fromChartParsers[L, W](grammar: Grammar[L], parsers: (SimpleChartParser[L, W])*) = {
    val infs = parsers.map{ p => 
      new LatentParserInference({(a:BinarizedTree[L], b:Seq[W])=>a},
        p.builder.withCharts(ParseChart.logProb),
        p.projections)
    }
    new EPParser(grammar, epinf)
  }
  */
}