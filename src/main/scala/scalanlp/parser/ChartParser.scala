package scalanlp.parser

import projections.{ProjectingSpanScorer, ProjectionIndexer}
import scalanlp.trees.BinarizedTree

/**
 * 
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class ChartParser[C,F,W](val builder: ChartBuilder[ParseChart,F,W],
                         val decoder: ChartDecoder[C,F,W],
                         val projections: ProjectionIndexer[C,F],
                         downWeightProjections:Boolean =true) extends Parser[C,W] {

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

    new ChartParser[L,L,W](builder, new MaxConstituentDecoder(ProjectionIndexer.simple(builder.grammar.index)), ProjectionIndexer.simple(builder.grammar.index));
  }

}
