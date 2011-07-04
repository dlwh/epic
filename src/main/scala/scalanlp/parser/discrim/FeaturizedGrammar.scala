package scalanlp.parser;
package discrim;

import scalala.tensor.dense._
import scalanlp.collection.mutable.SparseArrayMap;
import scalala.tensor.sparse._;
import scalanlp.parser.Grammar
import scalanlp.tensor.sparse.OldSparseVector
import scalala.tensor.mutable.Counter2

object FeaturizedGrammar {
  def apply[L,W](weights: DenseVector[Double], features: FeatureIndexer[L,W]) = {
    val unaryRules = Counter2[L,UnaryRule[L],Double]()

    for( (bRules,a) <- features.unaryRuleCache.iterator.zipWithIndex; (b,f) <- bRules) {
      val score = f dot weights;
      val rule = UnaryRule(features.labelIndex.get(a), features.labelIndex.get(b))
      unaryRules(rule.parent, rule) = score
    }

    val binaryRules = Counter2[L,BinaryRule[L],Double]()
    for( (bRules,a) <- features.binaryRuleCache.iterator.zipWithIndex; (b,cRules) <- bRules; (c,f) <- cRules) {
      val score = f dot weights;
      val rule = BinaryRule(features.labelIndex.get(a), features.labelIndex.get(b), features.labelIndex.get(c))
      binaryRules(rule.parent, rule) = score
    }

    Grammar(features.labelIndex, binaryRules, unaryRules)
  }
}
