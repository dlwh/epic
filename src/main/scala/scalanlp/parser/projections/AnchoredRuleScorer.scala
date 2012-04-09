package scalanlp.parser
package projections

import java.io._
import scalanlp.collection.mutable.TriangularArray
import scalanlp.tensor.sparse.OldSparseVector
import scalanlp.util.TypeTags.{tag,ID}
import scalanlp.trees.Rule

/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
case class AnchoredPCFGProjector[L, W](grammar: Grammar[L], threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {

  type MyScorer = AnchoredRuleScorer[L];
  private def normalize(ruleScores: OldSparseVector, totals: OldSparseVector):OldSparseVector = {
    if(ruleScores eq null) null
    else {
      val r = new OldSparseVector(ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize * 3 / 2)
      for( (rule, score) <- ruleScores.activeIterator) {
        val parent = grammar.parent(tag[Rule[L]](rule))
        if(score > 0)
          r(rule) = math.log(score) - math.log(totals(parent))
//        r(rule) = score - totals(parent)
      }
      r
    }
  }

  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totalsBinaries) = ruleData;
    val normUnaries:Array[OldSparseVector] = for((ruleScores, totals) <- unaryScores zip totalsUnaries) yield {
      normalize(ruleScores, totals)
    }

    val normBinaries:Array[Array[OldSparseVector]] = for ((splits, totals) <- binaryScores zip totalsBinaries) yield {
      if(splits eq null) null
      else for(ruleScores <- splits) yield normalize(ruleScores, totals)
    }
    new AnchoredRuleScorer(lexicalScores, normUnaries, normBinaries);
  }

}



/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
case class AnchoredRuleMarginalProjector[L, W](threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {
  private def normalize(ruleScores: OldSparseVector):OldSparseVector = {
    if(ruleScores eq null) null
    else {
      val r = new OldSparseVector(ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule, score) <- ruleScores.pairsIterator) {
        r(rule) = math.log(score)
      }
      r
    }
  }

  type MyScorer = AnchoredRuleScorer[L];
  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, _, binaryScores, _) = ruleData;
    val normUnaries:Array[OldSparseVector] = for(ruleScores <- unaryScores) yield {
      normalize(ruleScores)
    }

    val normBinaries:Array[Array[OldSparseVector]] = for (splits <- binaryScores) yield {
      if(splits eq null) null
      else for(ruleScores <- splits) yield normalize(ruleScores)
    }
    new AnchoredRuleScorer(lexicalScores, normUnaries, normBinaries);
  }

}


@SerialVersionUID(3)
class AnchoredRuleScorer[L](spanScores: Array[OldSparseVector], // triangular index -> label -> score
                            // (begin, end) -> rule -> score
                            unaryScores: Array[OldSparseVector],
                            // (begin, end) -> (split-begin) -> rule -> score
                            binaryScores: Array[Array[OldSparseVector]]) extends SpanScorer[L] with Serializable {

  def scoreUnaryRule(begin: Int, end: Int, rule: ID[Rule[L]]) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else forSpan(rule)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: ID[Rule[L]]) = {
    val forSpan = binaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else {
      val forSplit = forSpan(split - begin)
      if(forSplit eq null) Double.NegativeInfinity
      else forSplit(rule)
    }
  }
  def scoreSpan(begin: Int, end: Int, tag: ID[L]): Double = {
    val scores = spanScores(TriangularArray.index(begin, end))
    if(scores ne null) scores(tag)
    else Double.NegativeInfinity
  }
}

