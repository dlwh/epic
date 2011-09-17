package scalanlp.parser

import projections.{GrammarProjections, ProjectingSpanScorer, ProjectionIndexer}
import scalanlp.trees.BinarizedTree

/**
 * A ChartParser produces trees with labels C from a ChartBuilder with labels F, a decoder from C to F, and
 * projections from C to F
 * @author dlwh
 */
@SerialVersionUID(1)
class ChartParser[C,F,W](val builder: ChartBuilder[ParseChart,F,W],
                         val decoder: ChartDecoder[C,F,W],
                         val projections: GrammarProjections[C,F],
                         downWeightProjections:Boolean =true) extends Parser[C,W] with Serializable {

  override def bestParse(w: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity):BinarizedTree[C] = try {
    val metaScorer = new ProjectingSpanScorer(projections,scorer,downWeightProjections);
    val chart = builder.buildInsideChart(w, validSpan = metaScorer);
    lazy val outsideChart = builder.buildOutsideChart(chart, validSpan =metaScorer)
    val bestParse = decoder.extractBestParse(builder.root, builder.grammar, chart, outsideChart, w, metaScorer);
    bestParse
  } catch {
    case e => throw e;
  }

}

object ChartParser {
  def apply[L,W](builder: ChartBuilder[ParseChart,L,W]) = {

    new ChartParser[L,L,W](builder,
      new MaxConstituentDecoder(GrammarProjections.identity(builder.grammar)), GrammarProjections.identity(builder.grammar))
  }

}
