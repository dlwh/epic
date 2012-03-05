package scalanlp.parser.lex

import java.util.Arrays
import scalanlp.parser._
import projections.{ProjectionIndexer, GrammarProjections}

/**
 * Just projects chart scores
 * @author dlwh
 */
object LexParseProjector {
  def projectChart[Chart[X]<:ParseChart[X], L](chart: Chart[L],
                                               chartFactory: ParseChart.Factory[Chart]):Chart[L] = {
    val newChart = chartFactory.apply(chart.grammar, chart.length, lexicalize=false)
    val scoreBuf = Array.ofDim[Double](chart.grammar.index.size,chart.length)
    val offsets = Array.ofDim[Int](chart.grammar.index.size)
    for (beg <- 0 until chart.length; end <- (beg + 1) to chart.length) {
      Arrays.fill(offsets,0)
      for (ah <- chart.top.enteredLabelIndexes(beg,end)) {
        val labelPart = chart.top.decodeLabelPart(ah)
        scoreBuf(labelPart)(offsets(labelPart)) = chart.top.labelScore(beg,end,ah)
        offsets(labelPart) += 1
      }
      for( a <- 0 until scoreBuf.length) {
        val off = offsets(a)
        if(off > 0) {
          newChart.top.enter(beg,end,a,scoreBuf(a),off)
        }
      }

      Arrays.fill(offsets,0)
      for (ah <- chart.bot.enteredLabelIndexes(beg,end)) {
        val labelPart = chart.bot.decodeLabelPart(ah)
        scoreBuf(labelPart)(offsets(labelPart)) = chart.bot.labelScore(beg,end,ah)
        offsets(labelPart) += 1
      }
      for( a <- 0 until scoreBuf.length) {
        val off = offsets(a)
        if(off > 0) {
          newChart.bot.enter(beg,end,a,scoreBuf(a),off)
        }
      }
    }
    newChart
  }
}

class LexChartParser[L,W](baseGrammar: Grammar[L],
                          builder: LexChartBuilder[ParseChart,L,W]) extends ChartParser[L,L,W] {
  def charts(w: Seq[W], scorer: SpanScorer[L]) = {
    val inside = builder.buildInsideChart(w, scorer)
    val outside = builder.buildOutsideChart(inside, w, scorer)
    val pi = LexParseProjector.projectChart(inside,builder.chartFactory)
    val po = LexParseProjector.projectChart(outside,builder.chartFactory)
    ChartPair(pi,po,pi.top.labelScore(0,inside.length,builder.root), scorer)
  }

  val decoder = new MaxConstituentDecoder[L,L,W](GrammarProjections.identity(baseGrammar))
//  val decoder = new ViterbiDecoder[L,L,W](ProjectionIndexer.simple(baseGrammar.labelIndex))

  def projections = GrammarProjections.identity(baseGrammar)

  def root = builder.root

  protected def grammar = baseGrammar
}