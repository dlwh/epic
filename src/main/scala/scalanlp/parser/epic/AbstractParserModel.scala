package scalanlp.parser.epic

import scalala.tensor.dense.DenseVector
import scalanlp.parser.InsideOutside.{ExpectedCounts=>TrueCounts}
import scalanlp.parser._
import projections.{ProjectingSpanScorer, GrammarProjections}


/**
 * 
 * @author dlwh
 */

trait AbstractParserModel[L,L2,W] extends Model[TreeInstance[L,W]] {
  type ExpectedCounts = ParserExpectedCounts[W]
  type Inference <: ParserInference[L,L2, W]

  def extractParser(weights: DenseVector[Double]):Parser[L,W]

}

trait ParserInference[L,L2,W] extends MarginalInference[TreeInstance[L,W],SpanScorer[L]] {
  type ExpectedCounts = ParserExpectedCounts[W]
  type Marginal = ChartPair[ParseChart.LogProbabilityParseChart, L2]

  def projections: GrammarProjections[L,L2]
  def builder: ChartBuilder[ParseChart.LogProbabilityParseChart,L2,W]

  def marginal(v: TreeInstance[L, W], aug: SpanScorer[L]) = {
    val meta = new ProjectingSpanScorer(projections,aug)
    val inside = builder.buildInsideChart(v.words,meta)
    val outside = builder.buildOutsideChart(inside,meta)
    val root_score = inside.top.labelScore(0,v.words.length,builder.root)
    new ChartPair[ParseChart.LogProbabilityParseChart,L2](inside,outside,root_score,meta) -> root_score
  }

  def guessCountsFromMarginals(v: TreeInstance[L, W], marg: Marginal, aug: SpanScorer[L]) = {
    val root_score = marg.inside.top.labelScore(0,v.words.length,builder.root)
    val ec = new InsideOutside(builder).expectedCounts(v.words, marg.inside,
      marg.outside, root_score, marg.scorer, AnchoredSpanVisitor.noOp)
    new ParserExpectedCounts[W](ec)
  }

  def baseAugment(v: TreeInstance[L,W]) = v.spanScorer
}


case class ParserExpectedCounts[W](trueCounts: TrueCounts[W]) extends ExpectedCounts[ParserExpectedCounts[W]] {
  def +=(other: ParserExpectedCounts[W]) = {
    trueCounts += other.trueCounts
    this
  }

  def -=(other: ParserExpectedCounts[W]) = {
    trueCounts -= other.trueCounts
    this
  }

  def loss = trueCounts.logProb
}