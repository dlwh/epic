package scalanlp.parser.epic

import scalanlp.inference.Factor
import scalala.tensor.dense.DenseVector
import scalanlp.parser._
import projections.{AnchoredRuleScorerFactory, GrammarProjections, ScalingSpanScorer}
import scalanlp.parser.ParseChart._

case class SpanScorerFactor[L,W](f0Builder: ChartBuilder[LogProbabilityParseChart,L,W],
                                 words: Seq[W],
                                 scorer: SpanScorer[L]) extends Factor[SpanScorerFactor[L, W]] {
  def *(f: SpanScorerFactor[L, W]) = copy(scorer=SpanScorer.sum(scorer,f.scorer))

  def /(f: SpanScorerFactor[L, W]) = copy(scorer=ScalingSpanScorer(scorer, f.scorer, 0.0, -1))

  def *(f: Double) =  this

  // TODO: decide if we need this.
  // It's not strictly necessary, I think, but...
  def logPartition = {
    (f0Builder.buildInsideChart(words, scorer).top.labelScore(0,words.length,f0Builder.root))
  }

  def isConvergedTo(other: SpanScorerFactor[L,W], difference: Double = 1E-4):Boolean = {
    val length = words.length
    val scorer1 = scorer
    val scorer2 = other.scorer

    for { span <- (1 to length) } {
      var i = 0;
      while(i < length-span) {
        val j = i + span
        for(p <- (0 until f0Builder.grammar.labelIndex.size).iterator if !scorer.scoreSpan(i,j,p).isNegInfinity) {
          var k = i + 1
          while(k < j) {
            val rules = f0Builder.grammar.indexedBinaryRulesWithParent(p)
            var br = 0;
            while(br < rules.length) {
              val r = rules(br)
              br += 1
              // TODO: compute change that is consistent with all span scorers :-/
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

trait EPProjector[L,L2,W] {
  def coarseParser: ChartBuilder[LogProbabilityParseChart, L, W]

  lazy val zero = new CKYChartBuilder[ParseChart.LogProbabilityParseChart, L,W](coarseParser.root,
    new ZeroLexicon(coarseParser.lexicon), Grammar.zero(coarseParser.grammar),ParseChart.logProb)


  def project(inf: ParserInference[L,L2,W],
              projections: GrammarProjections[L,L2],
              instance: TreeInstance[L, W],
              marginal: ChartPair[ParseChart.LogProbabilityParseChart,L2],
              oldScorer: SpanScorer[L]):SpanScorer[L]
}

@SerialVersionUID(1)
class AnchoredRuleApproximator[C,F,W](val coarseParser: ChartBuilder[LogProbabilityParseChart,C,W],
                                      pruningThreshold: Double = Double.NegativeInfinity) extends EPProjector[C,F,W] with Serializable {

  val zeroFactory = new CachingSpanScorerFactory[C,W](coarseParser, pruningThreshold);

  def project(inf: ParserInference[C,F,W],
              projections: GrammarProjections[C,F],
              instance: TreeInstance[C, W],
              marginal: ChartPair[ParseChart.LogProbabilityParseChart,F],
              oldScorer: SpanScorer[C]):SpanScorer[C] = {
    val factory = new AnchoredRuleScorerFactory[C,F,W](coarseParser.grammar, SimpleChartParser(inf.builder,projections), pruningThreshold);
    factory.buildSpanScorer(marginal)
  }

}

