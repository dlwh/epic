package scalanlp.parser
package discrim

import ParseChart._
import projections._
import scalanlp.trees.BinarizedTree


/**
 * 
 * @author dlwh
 */
@SerialVersionUID(1)
trait EPApproximator[C,F,W] extends Serializable {
  def project(inside: ParseChart[F], outside: ParseChart[F], partition: Double, spanScorer: SpanScorer[F], goldTree: BinarizedTree[C] = null):SpanScorer[C]
  def divide(num: SpanScorer[C], denom: SpanScorer[C], words: Seq[W]):SpanScorer[C]
}


@SerialVersionUID(1)
class AnchoredRuleApproximator[C,F,W](fineParser: ChartBuilder[LogProbabilityParseChart,F,W],
                                      coarseParser: ChartBuilder[LogProbabilityParseChart,C,W],
                                      projections: GrammarProjections[C,F], pruningThreshold: Double = Double.NegativeInfinity) extends EPApproximator[C,F,W] with Serializable {
  val factory = new AnchoredRuleScorerFactory[C,F,W](coarseParser.grammar, new SimpleChartParser(fineParser,new ViterbiDecoder[C,F,W](projections.labels),projections), pruningThreshold);

  val zeroFactory = new CachingSpanScorerFactory[C,W](coarseParser, pruningThreshold);

  def project(inside: ParseChart[F], outside: ParseChart[F], partition: Double, spanScorer: SpanScorer[F], tree: BinarizedTree[C] = null):SpanScorer[C] = {
    val pruner = if(tree != null) {
      GoldTagPolicy.goldTreeForcing[C](tree.map(coarseParser.index))
    } else {
      GoldTagPolicy.noGoldTags[C]
    }
    factory.buildSpanScorer(new ChartPair[ParseChart,F](inside, outside,spanScorer),  partition, pruner)
  }

  def divide(num: SpanScorer[C], denom: SpanScorer[C], words: Seq[W]):SpanScorer[C] ={
    val div = ScalingSpanScorer(num,denom,0.0,-1);
    zeroFactory.mkSpanScorer(words,div);
  }
}
