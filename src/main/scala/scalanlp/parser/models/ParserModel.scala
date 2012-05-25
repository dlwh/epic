package scalanlp.parser
package models

import scalanlp.epic._
import scalala.tensor.Counter2
import scalanlp.parser.GenerativeParser
import scalanlp.trees.{TreeInstance, UnaryRule, BinaryRule}
import scalala.tensor.dense.DenseVector

/**
 * Base trait for "normal" Parser-type models
 * @author dlwh
 */
trait ParserModel[L, W] extends Model[TreeInstance[L, W]] with ParserExtractable[L, W] {
  type ExpectedCounts = scalanlp.parser.ExpectedCounts[Feature]
  type Inference <: ParserInference[L, W]

  def extractParser(weights: DenseVector[Double]) = {
    val inf = inferenceFromWeights(weights)
    SimpleChartParser(AugmentedGrammar(inf.grammar, inf.baseMeasure))
  }
}



trait ParserInference[L, W] extends ProjectableInference[TreeInstance[L, W], CoreAnchoring[L, W]] {
  type ExpectedCounts = scalanlp.parser.ExpectedCounts[Feature]
  type Marginal = ChartMarginal[ParseChart.LogProbabilityParseChart, L, W]

  def grammar: RefinedGrammar[L, W]
  def featurizer: RefinedFeaturizer[L, W, Feature]
  def baseMeasure: CoreGrammar[L, W]

  def marginal(v: TreeInstance[L, W], aug: CoreAnchoring[L, W]) = {
    val fullGrammar = AugmentedAnchoring(grammar.specialize(v.words), baseMeasure.specialize(v.words) * aug)
    val charts = fullGrammar.marginal
    charts -> charts.partition
  }

  def guessCountsFromMarginals(v: TreeInstance[L, W], marg: Marginal, aug: CoreAnchoring[L, W]) = {
    marg.expectedCounts(featurizer)
  }

  def baseAugment(v: TreeInstance[L, W])  = CoreAnchoring.identity(grammar.grammar, grammar.lexicon, v.words)

  protected def projector: EPProjector[L, W] = new AnchoredRuleApproximator(Double.NegativeInfinity)

  def project(v: TreeInstance[L, W], m: Marginal, oldAugment: CoreAnchoring[L, W]) = {
    projector.project(this, v, m)
  }
}

trait ParserModelFactory[L, W] extends ParserExtractableModelFactory[L, W] {
  type MyModel <: ParserModel[L, W]

  protected def extractBasicCounts[L, W](trees: IndexedSeq[TreeInstance[L, W]]): (Counter2[L, W, Double], Counter2[L, BinaryRule[L], Double], Counter2[L, UnaryRule[L], Double]) = {
    GenerativeParser.extractCounts(trees)
  }
}