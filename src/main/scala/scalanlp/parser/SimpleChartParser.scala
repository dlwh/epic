package scalanlp.parser

import projections.{GrammarProjections, ProjectingSpanScorer, ProjectionIndexer}
import scalanlp.trees.BinarizedTree

case class ChartPair[+PC[X]<:ParseChart[X],L](inside: PC[L],
                                              outside: PC[L],
                                              partition: Double,
                                              scorer: SpanScorer[L] = SpanScorer.identity)

@SerialVersionUID(1)
trait ChartParser[C,F,W] extends Parser[C,W] with Serializable {
  def charts(w: Seq[W], scorer: SpanScorer[C]= SpanScorer.identity):ChartPair[ParseChart,F]

  def decoder: ChartDecoder[C,F,W]

  def projections: GrammarProjections[C,F]
  def root: F
  protected def grammar: Grammar[F]

  override def bestParse(w: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity):BinarizedTree[C] = try {
    val chart = charts(w,scorer)
    val bestParse = decoder.extractBestParse(root, grammar, chart.inside, chart.outside, w, chart.scorer);
    bestParse
  } catch {
    case e => throw e;
  }
}

/**
 * A SimpleChartParser produces trees with labels C from a ChartBuilder with labels F, a decoder from C to F, and
 * projections from C to F
 * @author dlwh
 */
@SerialVersionUID(1)
class SimpleChartParser[C,F,W](val builder: ChartBuilder[ParseChart,F,W],
                         val decoder: ChartDecoder[C,F,W],
                         val projections: GrammarProjections[C,F]) extends ChartParser[C,F,W] with Serializable {

  def charts(w: Seq[W], scorer: SpanScorer[C]) = {
    val meta = new ProjectingSpanScorer[C,F](projections,scorer)
    val inside = builder.buildInsideChart(w,meta)
    val outside = builder.buildOutsideChart(inside,meta)
    val partition = inside.top.labelScore(0, inside.length, builder.root)
    new ChartPair[ParseChart,F](inside, outside, partition, meta)
  }

  def root = builder.root
  protected def grammar = builder.grammar

}

object SimpleChartParser {
  def apply[L,W](builder: ChartBuilder[ParseChart,L,W]) = {
    new SimpleChartParser[L,L,W](builder,
      new MaxConstituentDecoder(GrammarProjections.identity(builder.grammar)), GrammarProjections.identity(builder.grammar))
  }

  def apply[L,L2,W](builder: ChartBuilder[ParseChart,L2,W], proj: GrammarProjections[L,L2]) = {
    new SimpleChartParser[L,L2,W](builder, new MaxConstituentDecoder(proj), proj)
  }
}
