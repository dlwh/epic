package scalanlp.parser
package projections



import java.io._;

/**
 * Takes another SpanScorer.Factory, and thresholds its outputs so that any thing > threshold is 0.0, and
 * anything else is Double.NegativeInfinity
 *
 * @author dlwh
 */
class StepFunctionSpanScorerFactory[L,W](innerFactory: SpanScorer.Factory[L,L,W], threshold: Double= -7) extends SpanScorer.Factory[L,L,W] {
  def mkSpanScorer(s: scala.Seq[W], oldScorer: SpanScorer[L] = SpanScorer.identity):SpanScorer[L] = {
    val inner = innerFactory.mkSpanScorer(s,oldScorer);
    new StepFunctionSpanScorer(inner, threshold);
  }
}

@SerialVersionUID(1)
final class StepFunctionSpanScorer[L](inner: SpanScorer[L], threshold: Double = -7) extends SpanScorer[L] with Serializable {
  @inline def I(score: Double) = if(score > threshold) 0.0 else Double.NegativeInfinity;

  def scoreSpan(begin: Int, end: Int, tag: Int) = I(inner.scoreSpan(begin,end,tag))

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = I(inner.scoreUnaryRule(begin,end,rule));

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    I(inner.scoreBinaryRule(begin, split, end, rule))
  }
}

object StepFunctionifySpanScorers {
  import ProjectTreebankToLabeledSpans._;

  def main(args: Array[String]) {
    val outDir = new File(args(0));
    val inSpanDir = new File(args(1));

    val trainSpans = loadSpansFile(new File(inSpanDir,TRAIN_SPANS_NAME))
    val testSpans = loadSpansFile(new File(inSpanDir,TEST_SPANS_NAME))
    val devSpans = loadSpansFile(new File(inSpanDir,DEV_SPANS_NAME))

    outDir.mkdirs();

    writeIterable(trainSpans.map(new StepFunctionSpanScorer(_,Double.NegativeInfinity)),new File(outDir,TRAIN_SPANS_NAME))
    writeIterable(testSpans.map(new StepFunctionSpanScorer(_,Double.NegativeInfinity)),new File(outDir,TEST_SPANS_NAME))
    writeIterable(devSpans.map(new StepFunctionSpanScorer(_,Double.NegativeInfinity)),new File(outDir,DEV_SPANS_NAME))
  }

}
