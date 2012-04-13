package scalanlp.parser
package projections

import scalanlp.trees._
import scalanlp.collection.mutable.TriangularArray

import java.io._
import scalanlp.tensor.sparse.OldSparseVector
import scalala.library.Numerics

/**
 * Creates labeled span scorers for a set of trees from some parser.
 *
 * TODO: make this correct by using score - log(1-exp(score)) as the score returned (as in notes)
 * @author dlwh
class LabeledSpanScorerFactory[C,L,W](parser: ChartParser[C,L,W], threshold: Double) extends SpanScorer.Factory[C,L,W] {
  def indexedProjections = parser.projections.labels

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity, goldTag: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags) = {
    val charts = parser.charts(s,scorer)

    val sentProb = charts.inside.top.labelScore(0,s.length,parser.root)
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildScorer(charts,sentProb, scorer, goldTag)

    chartScorer
  }

  def buildScorer(charts: ChartMarginal[ParseChart,L], sentProb: Double,
                      coarseScorer: SpanScorer[C] = SpanScorer.identity,
                      goldTag: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags):LabeledSpanScorer[C] = {
    import charts._

    val scores = TriangularArray.raw(inside.length+1,null:OldSparseVector)
    for(begin <-  0 until inside.length; end <- begin+1 to (inside.length)) {
      val index = TriangularArray.index(begin, end)
      val scoresForLocation = indexedProjections.coarseEncoder.mkOldSparseVector(Double.NegativeInfinity)
      for(l <- inside.bot.enteredLabelIndexes(begin,end)) {
        val pL = indexedProjections.project(l)
        val myScore = inside.bot.labelScore(begin, end, l) + outside.bot.labelScore(begin, end, l) - sentProb
        val currentScore = scoresForLocation(pL)
        scoresForLocation(pL) = Numerics.logSum(currentScore,myScore)
      }

      for( (c,v) <- scoresForLocation.nonzero.pairs) {
        if(v > threshold || goldTag.isGoldTag(begin,end,c)) {
          if(scores(index) eq null) {
            scores(index) = indexedProjections.coarseEncoder.mkOldSparseVector(Double.NegativeInfinity)
          }
          scores(index)(c) = v
        }
      }

    }
    //println("Density: " + density * 1.0 / scores.length)
    //println("Label Density:" + labelDensity * 1.0 / scores.length / parser.grammar.index.size)
    new LabeledSpanScorer[C](scores)
  }

}

@SerialVersionUID(1)
class LabeledSpanScorer[L](scores: Array[OldSparseVector]) extends SpanScorer[L] with Serializable {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

  def scoreSpan(begin: Int, end: Int, label: Int): Double = {
    if(scores(TriangularArray.index(begin,end)) eq null) Double.NegativeInfinity
    else scores(TriangularArray.index(begin,end))(label)
  }
}


*/