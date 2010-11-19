package scalanlp.parser

import scalanlp.trees._;

/**
 * 
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class ChartParser[F,C,W](val builder: ChartBuilder[ParseChart,F,W],
                         decoder: ChartDecoder[C,F],
                         val scorerFactory: SpanScorer.Factory[W] = SpanScorer.identityFactory[W]) extends Parser[C,W] {

  override def bestParse(w: Seq[W]) = try {
    val scorer = scorerFactory.mkSpanScorer(w);
    val chart = builder.buildInsideChart(w, validSpan = scorer);
    lazy val outsideChart = builder.buildOutsideChart(chart, validSpan = scorer)
    val bestParse = decoder.extractBestParse(builder.root, builder.grammar, chart, outsideChart, scorer);
    bestParse
  } catch {
    case e => throw e;
  }
}

object ChartParser {
  def apply[L,W](builder: ChartBuilder[ParseChart,L,W]) = {
    new ChartParser[L,L,W](builder,SimpleViterbiDecoder(builder.grammar));
  }
}