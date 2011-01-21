package scalanlp.parser



/**
 * 
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class ChartParser[C,F,W](val builder: ChartBuilder[ParseChart,F,W],
                         val decoder: ChartDecoder[C,F],
                         val scorerFactory: SpanScorer.Factory[F,C,W] = SpanScorer.identityFactory[F,C,W]) extends Parser[C,W] {

  override def bestParse(w: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity) = try {
    val metaScorer = scorerFactory.mkSpanScorer(w, scorer);
    val chart = builder.buildInsideChart(w, validSpan = metaScorer);
    lazy val outsideChart = builder.buildOutsideChart(chart, validSpan =metaScorer)
    val bestParse = decoder.extractBestParse(builder.root, builder.grammar, chart, outsideChart, metaScorer);
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
