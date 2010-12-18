package scalanlp.parser
package projections

import scalanlp.trees._
import scalanlp.math.Numerics
import scalanlp.collection.mutable.{TriangularArray}
import scalala.tensor.sparse.SparseVector;
import scalala.tensor.Vector;

import java.io._
import scalanlp.concurrent.ParallelOps._
import scalanlp.trees.DenseTreebank

/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRuleScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                     indexedProjections: ProjectionIndexer[C,L],
                                     pruningThreshold: Double = -7)
        extends ChartDrivenScorerFactory[C,L,W](parser,indexedProjections,pruningThreshold) {

  type MyScorer = AnchoredRuleScorer;
  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores,logTotalsUnaries,binaryScores,logTotals) = ruleData;
    new AnchoredRuleScorer(lexicalScores, unaryScores, binaryScores, logTotals, logTotalsUnaries);
  }

}



/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
class AnchoredRulePosteriorScorerFactory[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                     indexedProjections: ProjectionIndexer[C,L],
                                     pruningThreshold: Double = -7)
        extends ChartDrivenScorerFactory[C,L,W](parser,indexedProjections,pruningThreshold) {

  type MyScorer = AnchoredRuleScorer;
  protected def createSpanScorer(ruleData: AnchoredRuleProjector.AnchoredData, sentProb: Double) = {
    val zeroSparseVector = new SparseVector(indexedProjections.coarseIndex.size, 0);
    zeroSparseVector.default = 0.0;
    val logTotals = TriangularArray.raw(ruleData.lexicalScores.length+1,zeroSparseVector);
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores, _, binaryScores, _) = ruleData;
    new AnchoredRuleScorer(lexicalScores, unaryScores, binaryScores, logTotals, logTotals);
  }

}


@serializable
@SerialVersionUID(2)
class AnchoredRuleScorer(lexicalScores: Array[SparseVector], // begin -> label -> score
                         // (begin,end) -> parent -> child -> score
                         unaryScores: Array[Array[SparseVector]],
                         // (begin,end) -> (split-begin) -> parent -> lchild -> rchild -> score
                         // so many arrays.
                         binaryScores: Array[Array[Array[Array[SparseVector]]]],
                         logTotalBinaries: Array[SparseVector], // sum of scores for binary scores.
                         logTotalUnaries: Array[SparseVector] // sum of scores for unary scores.
                        ) extends SpanScorer {

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else if(forSpan(parent) eq null) Double.NegativeInfinity
    else if(logTotalUnaries(TriangularArray.index(begin,end)) eq null)  Double.NegativeInfinity
    else {
      val r= forSpan(parent)(child) - logTotalUnaries(TriangularArray.index(begin,end))(parent);
      if(logTotalUnaries(TriangularArray.index(begin,end))(parent) == Double.NegativeInfinity) Double.NegativeInfinity
      else r
    }
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    val forSpan = binaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else {
      val forSplit = forSpan(split - begin)
      if(forSplit eq null) Double.NegativeInfinity
      else {
        val forParent = forSplit(parent)
        if(forParent == null || forParent(leftChild) == null) Double.NegativeInfinity
        else {
          val r = forParent(leftChild)(rightChild) - logTotalBinaries(TriangularArray.index(begin,end))(parent);
          if(logTotalBinaries(TriangularArray.index(begin,end))(parent) == Double.NegativeInfinity) Double.NegativeInfinity
          else r

        }
      }
    }
  }
  def scoreLexical(begin: Int, end: Int, tag: Int): Double = {
    lexicalScores(begin)(tag);
  }
}

