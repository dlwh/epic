package scalanlp.parser
package epic

import scalala.tensor.dense.DenseVector
import scalanlp.trees.BinarizedTree

/**
 * Parser that runs EP using the EPInference object and extracts a parse
 *
 * @author dlwh
 */
class EPParser[L,W](zeroParser: SimpleChartParser[L,L,W],
                    inference: EPInference[TreeInstance[L,W], SpanScorerFactor[L,W]]) extends Parser[L,W] with Serializable {
  def bestParse(s: Seq[W], spanScorer: SpanScorer[L]) = {
    val inst = new TreeInstance("",null,s,spanScorer)
    val augment = inference.getMarginals(inst,new SpanScorerFactor(zeroParser.builder.withCharts(ParseChart.logProb),s,spanScorer))._2
    zeroParser.bestParse(s,augment.scorer)
  }

}

object EPParser {
  trait Extractor[L,W] extends EPModel[TreeInstance[L,W],SpanScorerFactor[L,W]] with ParserExtractable[L,W] {
    def zeroParser: SimpleChartParser[L,L,W]

    def extractParser(weights: DenseVector[Double]) = {
      new EPParser(zeroParser, inferenceFromWeights(weights))
    }
  }

  def fromChartParsers[L,W](coarseParser: SimpleChartParser[L, L, W], parsers: (SimpleChartParser[L,_,W])*) = {
    val infs = parsers.map{ p => 
      val pp = p.asInstanceOf[SimpleChartParser[L, AnyRef,  W]]
      new LatentParserInference({(a:BinarizedTree[L],b:Seq[W])=>a},
        coarseParser.builder.withCharts(ParseChart.logProb),
        pp.builder.withCharts(ParseChart.logProb),
        pp.projections)
    }
    val zeroParser = SimpleChartParser(new CKYChartBuilder(coarseParser.root,
      new ZeroLexicon(coarseParser.builder.lexicon),
      Grammar.zero(coarseParser.builder.grammar),ParseChart.logProb))
    val epinf = new EPInference(infs.toIndexedSeq, 5)
    new EPParser(zeroParser, epinf)
  }
}