package scalanlp.parser
package discrim

import ParseChart._
import scalanlp.collection.mutable.TriangularArray
import scalanlp.math.Numerics
import projections._
import scalanlp.trees.BinarizedTree


/**
 * 
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
trait EPApproximator[C,F,W] {
  // TODO: add a type for the kind of span scorer this is.
  def project(inside: ParseChart[F], outside: ParseChart[F], partition: Double, spanScorer: SpanScorer[F], goldTree: BinarizedTree[C]):SpanScorer[C]
  def divideAndNormalize(num: SpanScorer[C], denom: SpanScorer[C], words: Seq[W]):SpanScorer[C]
}


@serializable
@SerialVersionUID(1)
class AnchoredRuleApproximator[C,F,W](fineParser: ChartBuilder[LogProbabilityParseChart,F,W],
                                      coarseParser: ChartBuilder[LogProbabilityParseChart,C,W],
                                      projections: ProjectionIndexer[C,F], pruningThreshold: Double = Double.NegativeInfinity) extends EPApproximator[C,F,W] {
  val factory = new AnchoredRuleScorerFactory[C,F,W](fineParser,projections,pruningThreshold);

  val zeroFactory = new CachingSpanScorerFactory[C,W](coarseParser);

  def project(inside: ParseChart[F], outside: ParseChart[F], partition: Double, spanScorer: SpanScorer[F], tree: BinarizedTree[C]):SpanScorer[C] = {
    factory.buildSpanScorer(inside,outside,  partition, spanScorer, tree);
  }

  def divideAndNormalize(num: SpanScorer[C], denom: SpanScorer[C], words: Seq[W]):SpanScorer[C] ={
    val div = ScalingSpanScorer(num,denom,0.0,-1);
    zeroFactory.mkSpanScorer(words,div);
  }
}