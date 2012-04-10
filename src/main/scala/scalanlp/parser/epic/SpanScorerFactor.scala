package scalanlp.parser.epic

import scalanlp.inference.Factor
import scalanlp.parser._
import projections.{AnchoredPCFGProjector, ScalingSpanScorer}

case class SpanScorerFactor[L, W](grammar: Grammar[L],
                                  words: Seq[W],
                                  scorer: SpanScorer[L]) extends Factor[SpanScorerFactor[L, W]] {
  def *(f: SpanScorerFactor[L, W]) = copy(scorer=SpanScorer.sum(scorer, f.scorer))

  def /(f: SpanScorerFactor[L, W]) = copy(scorer=ScalingSpanScorer(scorer, f.scorer, 0.0, -1))

  def *(f: Double) =  this

  lazy val logPartition = {
    val marg = ChartMarginal.fromSentence(WeightedGrammar.oneOff[L,W](grammar, scorer), words)
    marg.partition
  }

  def isConvergedTo(other: SpanScorerFactor[L, W], difference: Double = 1E-4):Boolean = {
    val length = words.length
    val scorer1 = scorer
    val scorer2 = other.scorer


    /**
     * TODO: include at least spans in this computation
     */
    for { span <- (1 to length) } {
      var i = 0;
      while(i < length-span) {
        val j = i + span
        for(p <- (0 until grammar.labelIndex.size).iterator if !scorer.scoreSpan(i, j, p).isNegInfinity) {
          var k = i + 1
          while(k < j) {
            val rules = grammar.indexedBinaryRulesWithParent(p)
            var br = 0;
            while(br < rules.length) {
              val r = rules(br)
              br += 1
              val s1 = scorer1.scoreBinaryRule(i, k, j, r)
              val s2 = scorer2.scoreBinaryRule(i, k, j, r)
              val a = (s1 - s2)
              val diff = if(a.isNaN) 0.0 // from negative infinities...el
              else if(a < 0 && a.isInfinite) 1001.0
              else if(a.isInfinite) 10000.
              else if(s1.abs < 1E-4) a.abs
              else a.abs / (s1.abs + s2.abs) * 2
              if(diff > difference) {
                return false
              }
            }
            k += 1
          }
        }
        i += 1
      }
    }

    true
  }
}

trait EPProjector[L, W] {
  def project(inf: ParserInference[L, W],
              instance: TreeInstance[L, W],
              marginal: ChartMarginal[ParseChart.LogProbabilityParseChart, L, W]):WeightedGrammar[L, W]
}

@SerialVersionUID(1)
class AnchoredRuleApproximator[L, W](pruningThreshold: Double = Double.NegativeInfinity) extends EPProjector[L, W] with Serializable {

  def project(inf: ParserInference[L, W],
              instance: TreeInstance[L, W],
              marginal: ChartMarginal[ParseChart.LogProbabilityParseChart, L, W]):WeightedGrammar[L, W] = {
    val factory = new AnchoredPCFGProjector[L, W](marginal.grammar)
    WeightedGrammar.oneOff(inf.grammar.grammar, factory.buildSpanScorer(marginal))
  }

}

