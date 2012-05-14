package scalanlp.parser
package models

import scalanlp.epic._
import scalala.tensor.Counter2
import scalanlp.trees.{UnaryRule, BinaryRule}
import scalanlp.parser.{GenerativeParser, TreeInstance}

/**
 * Base trait for "normal" Parser-type models
 * @author dlwh
 */
trait ParserModel[L, W] extends Model[TreeInstance[L, W]] with ParserExtractable[L, W] {
  type ExpectedCounts = scalanlp.parser.ExpectedCounts[Feature]
  type Inference <: ParserInference[L, W]
}



trait ParserInference[L, W] extends ProjectableInference[TreeInstance[L, W], DerivationScorer[L, W]] {
  type ExpectedCounts = scalanlp.parser.ExpectedCounts[Feature]
  type Marginal = ChartMarginal[ParseChart.LogProbabilityParseChart, L, W]

  def grammar: DerivationScorer.Factory[L, W]
  def featurizer: DerivationFeaturizer[L, W, Feature]
  def baseMeasure: DerivationScorer.Factory[L, W]

  def marginal(v: TreeInstance[L, W], aug: DerivationScorer[L, W]) = {
    val fullGrammar = grammar.specialize(v.words) * baseMeasure.specialize(v.words) * aug
    val charts = fullGrammar.marginal
    charts -> charts.partition
  }

  def guessCountsFromMarginals(v: TreeInstance[L, W], marg: Marginal, aug: DerivationScorer[L, W]) = {
    marg.expectedCounts(featurizer)
  }

  def baseAugment(v: TreeInstance[L, W])  = DerivationScorer.identity(grammar.grammar, grammar.lexicon, v.words)

  protected def projector: EPProjector[L, W] = new AnchoredRuleApproximator(Double.NegativeInfinity)

  def project(v: TreeInstance[L, W], m: Marginal, oldAugment: DerivationScorer[L, W]) = {
    projector.project(this, v, m)
  }
}

trait ParserModelFactory[L, W] extends ParserExtractableModelFactory[L, W] {
  type MyModel <: ParserModel[L, W]

  protected def extractBasicCounts[L, W](trees: IndexedSeq[TreeInstance[L, W]]): (Counter2[L, W, Double], Counter2[L, BinaryRule[L], Double], Counter2[L, UnaryRule[L], Double]) = {
    GenerativeParser.extractCounts(trees)
  }
}