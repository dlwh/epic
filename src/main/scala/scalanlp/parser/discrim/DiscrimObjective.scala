package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalanlp.optimize.DiffFunction
import scalanlp.trees._;

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

  private def treeToFeatures(t: BinarizedTree[Int], words: Seq[W]):Iterator[SparseVector] = {
    for(t2 <- t.allChildren)
      yield t2 match {
        case BinaryTree(a,Tree(b,_),Tree(c,_)) =>
          indexedFeatures.featuresFor(a,b,c);
        case UnaryTree(a,Tree(b,_)) =>
          indexedFeatures.featuresFor(a,b);
        case n@NullaryTree(a) =>
          val w = words(n.span.start);
          indexedFeatures.featuresFor(a,w);
      }
  }


}