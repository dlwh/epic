package scalanlp.parser.epic

import scalala.tensor.dense.DenseVector
import scalanlp.parser.InsideOutside.{ExpectedCounts=>TrueCounts}
import scalanlp.parser._
import projections.{ProjectingSpanScorer, GrammarProjections}
import scalala.tensor.{Counter2,::}
import scalala.library.Library

/**
 * Base trait for Parser-type models
 * @author dlwh
 */
trait ParserModel[L,W] extends Model[TreeInstance[L,W]] {
  type L2 // refined label type
  type ExpectedCounts = ParserExpectedCounts[W]
  type Inference <: ParserInference[L, L2, W]

  def projections: GrammarProjections[L, L2]

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

trait ParserModelFactory[L,W] extends ModelFactory[TreeInstance[L,W]] {
  type MyModel <: ParserModel[L,W]

  // Various useful helpful methods
  protected def extractBasicCounts(trees: IndexedSeq[TreeInstance[L,W]]): (Counter2[L, W, Double], Counter2[L, BinaryRule[L], Double], Counter2[L, UnaryRule[L], Double]) = {
    GenerativeParser.extractCounts(trees)
  }

  protected def determineOpenTags[L,L2,W](initLexicon: Counter2[L, W, Double], indexedProjections: GrammarProjections[L, L2]): Set[L2] = {
    Set.empty ++ {
      for (t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t, ::).size > 50; t2 <- indexedProjections.labels.refinementsOf(t)) yield t2
    }
  }

  protected def determineKnownTags[L,L2,W](baseLexicon: Lexicon[L, W], indexedProjections: GrammarProjections[L, L2]): Set[(L2,W)] = {
    for ((t, w) <- baseLexicon.knownTagWords.toIterable; t2 <- indexedProjections.labels.refinementsOf(t)) yield (t2, w)
  }.toSet

  protected def determineClosedWords(initLexicon: Counter2[L, W, Double]): Set[W] = {
    Set.empty ++ {
      val wordCounts = Library.sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1)
    }
  }


}