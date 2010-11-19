package scalanlp.parser

import scalanlp.trees._;

/**
 * 
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class ChartParser[F,C,W](val builder: ChartBuilder[ParseChart,F,W], decoder: ChartDecoder[C,F]) extends Parser[C,W] {
  override def bestParse(w: Seq[W]) = try {
    val chart = builder.buildInsideChart(w);
    lazy val outsideChart = builder.buildOutsideChart(chart)
    val bestParse = decoder.extractBestParse(builder.root, builder.grammar, chart, outsideChart);
    bestParse
  } catch {
    case e =>
      throw e;
  }
}

object ChartParser {
  def apply[L,W](builder: ChartBuilder[ParseChart,L,W]) = {
    new ChartParser[L,L,W](builder,SimpleViterbiDecoder(builder.grammar));
  }
}