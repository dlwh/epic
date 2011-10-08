package scalanlp.parser

import projections.{GrammarProjections, ProjectingSpanScorer, ProjectionIndexer}
import scalanlp.trees.BinarizedTree

class ChartPair[+PC[X]<:ParseChart[X],L](val inside: PC[L], _outside: =>PC[L], val scorer: SpanScorer[L] = SpanScorer.identity) {
  lazy val outside = _outside
}

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
                         val projections: GrammarProjections[C,F],
                         downWeightProjections:Boolean =true) extends ChartParser[C,F,W] with Serializable {

  def charts(w: Seq[W], scorer: SpanScorer[C]) = {
    val meta = new ProjectingSpanScorer[C,F](projections,scorer,downWeightProjections)
    val inside = builder.buildInsideChart(w,meta)
    lazy val outside = builder.buildOutsideChart(inside,meta)
    new ChartPair[ParseChart,F](inside,outside,meta)
  }

  def root = builder.root
  protected def grammar = builder.grammar



}

object SimpleChartParser {
  def apply[L,W](builder: ChartBuilder[ParseChart,L,W]) = {
    new SimpleChartParser[L,L,W](builder,
      new MaxConstituentDecoder(GrammarProjections.identity(builder.grammar)), GrammarProjections.identity(builder.grammar))
  }

}
