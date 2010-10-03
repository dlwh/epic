package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalanlp.optimize.DiffFunction
import scalanlp.trees._;
import InsideOutside._;

/**
 * 
 * @author dlwh
 */
class DiscrimObjective[L,W] extends DiffFunction[Int,DenseVector] {
  def calculate(weights: DenseVector) = {
    val grad = weights.like;

    (Double.NegativeInfinity,grad);
  }

  val indexedFeatures: FeatureIndexer[L,W] = null;

  def weightsToGrammar(weights: DenseVector) = {
    val grammar = new FeaturizedGrammar(weights, indexedFeatures);
    grammar;
  }

  // these expected counts are in normal space, not log space.
  private def treeToExpectedCountsAndScore(g: Grammar[L],
                                   lexicon: Lexicon[L,W],
                                   t: BinarizedTree[Int],
                                   words: Seq[W]):(ExpectedCounts[W],Double) = {
    val expectedCounts = new ExpectedCounts[W](g)
    var score = 0.0;
    for(t2 <- t.allChildren) {
      t2 match {
        case BinaryTree(a,Tree(b,_),Tree(c,_)) =>
          expectedCounts.binaryRuleCounts(a)(b)(c) += 1
          score += g.binaryRuleScore(a,b,c);
        case UnaryTree(a,Tree(b,_)) =>
          expectedCounts.unaryRuleCounts(a)(b) += 1
          score += g.unaryRuleScore(a,b);
        case n@NullaryTree(a) =>
          val w = words(n.span.start);
          expectedCounts.wordCounts(a)(w) += 1
          score += lexicon.wordScore(g.index.get(a), w);
      }
    }
    (expectedCounts,score);
  }


}