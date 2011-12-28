package scalanlp.parser
package projections

import scalanlp.collection.mutable.TriangularArray
import java.io._
import scalanlp.tensor.sparse.OldSparseVector


/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRuleScorerFactory[C,L,W](val coarseGrammar: Grammar[C],
                                       parser: SimpleChartParser[C,L,W])
        extends ChartDrivenScorerFactory[C,L,W](coarseGrammar,parser) {

  type MyScorer = AnchoredRuleScorer[C];
  private def normalize(ruleScores: OldSparseVector, totals: OldSparseVector):OldSparseVector = {
    if(ruleScores eq null) null
    else {
      val r = new OldSparseVector(ruleScores.length,ruleScores.default,ruleScores.activeSize)
      for( (rule,score) <- ruleScores.activeIterator) {
        val parent = indexedProjections.labels.coarseIndex(indexedProjections.rules.coarseIndex.get(rule).parent)
        r(rule) = math.log(score) - math.log(totals(parent))
      }
      r
    }
  }

  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores,totalsUnaries,binaryScores,totalsBinaries) = ruleData;
    val normUnaries:Array[OldSparseVector] = for((ruleScores,totals) <- unaryScores zip totalsUnaries) yield {
      normalize(ruleScores, totals)
    }

    val normBinaries:Array[Array[OldSparseVector]] = for ((splits,totals) <- binaryScores zip totalsBinaries) yield {
      if(splits eq null) null
      else for(ruleScores <- splits) yield normalize(ruleScores,totals)
    }
    new AnchoredRuleScorer(lexicalScores, normUnaries, normBinaries);
  }

}



/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRulePosteriorScorerFactory[C,L,W](coarseGrammar: Grammar[C],
                                                parser: SimpleChartParser[C,L,W])
        extends ChartDrivenScorerFactory[C,L,W](coarseGrammar, parser) {

  type MyScorer = AnchoredRuleScorer[C];
  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores, _, binaryScores, _) = ruleData;
    val logUnaryScores = unaryScores.map(vec => if(vec eq null) null else vec.values.map(math.log))
    val logBinaryScores = binaryScores.map(arr => if(arr eq null) null else arr.map(vec => if(vec eq null) null else vec.values.map(math.log)))
    new AnchoredRuleScorer(lexicalScores, logUnaryScores, logBinaryScores);
  }

}


@SerialVersionUID(3)
class AnchoredRuleScorer[L](spanScores: Array[OldSparseVector], // triangular index -> label -> score
                         // (begin,end) -> rule -> score
                         unaryScores: Array[OldSparseVector],
                         // (begin,end) -> (split-begin) -> rule -> score
                         binaryScores: Array[Array[OldSparseVector]]) extends SpanScorer[L] with Serializable {

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else forSpan(rule)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val forSpan = binaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else {
      val forSplit = forSpan(split - begin)
      if(forSplit eq null) Double.NegativeInfinity
      else forSplit(rule)
    }
  }
  def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
    val scores = spanScores(TriangularArray.index(begin, end))
    if(scores ne null) scores(tag)
    else Double.NegativeInfinity
  }
}

