package scalanlp.parser.epic

import scalanlp.parser.InsideOutside.{ExpectedCounts=>TrueCounts}
import scalanlp.parser._
import projections.{ProjectingSpanScorer, GrammarProjections}
import scalala.tensor.{Counter2,::}
import scalala.library.Library

/**
 * Base trait for "normal" Parser-type models
 * @author dlwh
 */
trait ParserModel[L,W] extends Model[TreeInstance[L,W]] with ParserExtractable[L,W] {
  type L2 // refined label type
  type ExpectedCounts = ParserExpectedCounts[W]
  type Inference <: ParserInference[L, L2, W]
}

trait ParserInference[L,L2,W] extends ProjectableInference[TreeInstance[L,W],SpanScorerFactor[L,W]] {
  type ExpectedCounts = ParserExpectedCounts[W]
  type Marginal = ChartPair[ParseChart.LogProbabilityParseChart, L2]

  def projections: GrammarProjections[L,L2]
  def builder: ChartBuilder[ParseChart.LogProbabilityParseChart,L2,W]
  def coarseBuilder: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W]

  def marginal(v: TreeInstance[L, W], aug: SpanScorerFactor[L,W]) = {
    val meta = new ProjectingSpanScorer(projections,aug.scorer)
    val inside = builder.buildInsideChart(v.words,meta)
    val outside = builder.buildOutsideChart(inside,meta)
    val root_score = inside.top.labelScore(0,v.words.length,builder.root)
    new ChartPair[ParseChart.LogProbabilityParseChart,L2](inside,outside,root_score,meta) -> root_score
  }

  def guessCountsFromMarginals(v: TreeInstance[L, W], marg: Marginal, aug: SpanScorerFactor[L, W]) = {
    val root_score = marg.inside.top.labelScore(0,v.words.length,builder.root)
    val ec = new InsideOutside(builder).expectedCounts(v.words, marg.inside,
      marg.outside, root_score, marg.scorer, AnchoredSpanVisitor.noOp)
    new ParserExpectedCounts[W](ec)
  }

  def baseAugment(v: TreeInstance[L,W]) = new SpanScorerFactor(projector.zero, v.words, v.spanScorer)

  protected def projector: EPProjector[L,L2,W] = new AnchoredRuleApproximator(coarseBuilder, Double.NegativeInfinity)

  def project(v: TreeInstance[L, W], m: Marginal, oldAugment: SpanScorerFactor[L, W]) = {
    val p = projector
    new SpanScorerFactor(p.zero, v.words, p.project(this, projections, v, m, oldAugment.scorer))
  }
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

trait ParserModelFactory[L,W] extends ParserExtractableModelFactory[L,W] {
  type MyModel <: ParserModel[L,W]

  protected def extractBasicCounts[L,W](trees: IndexedSeq[TreeInstance[L,W]]): (Counter2[L, W, Double], Counter2[L, BinaryRule[L], Double], Counter2[L, UnaryRule[L], Double]) = {
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

  protected def determineClosedWords[L,W](initLexicon: Counter2[L, W, Double]): Set[W] = {
    Set.empty ++ {
      val wordCounts = Library.sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1)
    }
  }


}