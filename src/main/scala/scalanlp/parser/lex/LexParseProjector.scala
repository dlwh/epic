package scalanlp.parser.lex

import java.util.Arrays
import scalanlp.parser._
import projections.{ProjectionIndexer, GrammarProjections}
import scalanlp.collection.mutable.TriangularArray
import scalala.library.Numerics._
import scalanlp.trees._
import scalanlp.util._

/**
 * Just projects chart scores to get label marginals
 * @author dlwh
 */
object LexParseProjector {
  def projectChart[Chart[X]<:ParseChart[X], L](inside: Chart[L], outside: Chart[L], rootScore: Double,
                                               chartFactory: ParseChart.Factory[Chart]):Chart[L] = {
    val newChart = chartFactory.apply(inside.grammar, inside.length, lexicalize=false)
    val scoreBuf = Array.ofDim[Double](inside.grammar.index.size,inside.length)
    val offsets = Array.ofDim[Int](inside.grammar.index.size)
    for (beg <- 0 until inside.length; end <- (beg + 1) to inside.length) {
      Arrays.fill(offsets,0)
      for (ah <- inside.top.enteredLabelIndexes(beg,end)) {
        val labelPart = inside.top.decodeLabelPart(ah)
        scoreBuf(labelPart)(offsets(labelPart)) = inside.top.labelScore(beg,end,ah) + outside.top.labelScore(beg, end, ah) - rootScore
        offsets(labelPart) += 1
      }
      for( a <- 0 until scoreBuf.length) {
        val off = offsets(a)
        if(off > 0) {
          newChart.top.enter(beg,end,a,scoreBuf(a),off)
        }
      }

      Arrays.fill(offsets,0)
      for (ah <- inside.bot.enteredLabelIndexes(beg,end)) {
        val labelPart = inside.bot.decodeLabelPart(ah)
        scoreBuf(labelPart)(offsets(labelPart)) = inside.bot.labelScore(beg,end,ah) + outside.top.labelScore(beg, end, ah) - rootScore
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
                          builder: LexChartBuilder[ParseChart,L,W]) extends Parser[L,W] {
  def charts(w: Seq[W], scorer: SpanScorer[L]) = {
    val pair = builder.buildCharts(w,scorer)
    import pair._
    val pi = LexParseProjector.projectChart(pair.inside, pair.outside, pair.partition, builder.chartFactory)
    pi
  }



  def bestParse(s: Seq[W], spanScorer: SpanScorer[L]) = {
    val marginals = charts(s, spanScorer)
    val maxSplit = new TriangularArray[Int](marginals.length+1,0)
    val maxBotLabel = new TriangularArray[Int](marginals.length+1,-1)
    val maxBotScore = new TriangularArray[Double](marginals.length+1,Double.NegativeInfinity)
    val maxTopLabel = new TriangularArray[Int](marginals.length+1,-1)
    val maxTopScore = new TriangularArray[Double](marginals.length+1,Double.NegativeInfinity)
    def findArgmax(scores: marginals.ChartScores, beg: Int, end: Int) = {
      var argmax = 0
      var scoreMax = -10000.0
      for(l <- scores.enteredLabelIndexes(beg,end)) {
        val score = scores.labelScore(beg,end,l)
        if(score > scoreMax) {
          argmax = l
          scoreMax = score
        }
      }
      (argmax,scoreMax)
    }


    for(i <- 0 until marginals.length) {
      val (bam, bsm) = findArgmax(marginals.bot,i,i+1)
      maxBotLabel(i,i+1) = bam
      maxBotScore(i,i+1) = bsm

      val (tam, tsm) = findArgmax(marginals.top,i,i+1)
      maxTopLabel(i,i+1) = tam
      maxTopScore(i,i+1) = logSum(tsm,bsm)
    }

    for {
      span <- 2 to marginals.length
      begin <- 0 to (marginals.length - span)
      end = begin + span
    } {
      val (bam, bsm) = findArgmax(marginals.bot,begin,end)
      maxBotLabel(begin,end) = bam
      maxBotScore(begin,end) = bsm

      val (tam, tsm) = findArgmax(marginals.top,begin,end)
      maxTopLabel(begin,end) = tam
      maxTopScore(begin,end) = logSum(tsm,bsm)

      val (split,splitScore) = (for(split <- begin +1 until end) yield {
        val score = logSum(maxTopScore(begin,split),maxTopScore(split,end))
        (split,score)
      })reduceLeft ( (a,b) => if(a._2 > b._2) a else b)

      maxSplit(begin,end) = split
      maxTopScore(begin,end) = logSum(maxTopScore(begin,end),splitScore)
    }

    def extract(begin: Int, end: Int):BinarizedTree[L] = {
      val lower = if(begin + 1== end) {
        NullaryTree(baseGrammar.labelIndex.get(maxBotLabel(begin,end)))(Span(begin,end))
      } else {
        val split = maxSplit(begin,end)
        val left = extract(begin,split)
        val right = extract(split,end)
        BinaryTree(baseGrammar.labelIndex.get(maxBotLabel(begin,end)),left,right)(Span(begin,end))
      }

      UnaryTree(baseGrammar.labelIndex.get(maxTopLabel(begin,end)),lower)(Span(begin,end))
    }

    extract(0,marginals.length)
  }

}

