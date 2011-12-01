package scalanlp.parser;
package discrim;

import scalala.tensor.dense._
import scalanlp.parser.Grammar
import scalala.tensor.mutable.Counter2

object FeaturizedGrammar {
  def apply[L,W](weights: DenseVector[Double], features: FeatureIndexer[L,W]) = {
    val unaryRules = Counter2[L,UnaryRule[L],Double]()
    val binaryRules = Counter2[L,BinaryRule[L],Double]()

    for( (feats,r) <- features.ruleCache.iterator.zipWithIndex) {
      val score = feats dot weights;
      val rule = features.ruleIndex.get(r)
      rule match {
        case br@BinaryRule(_,_,_) =>
          binaryRules(br.parent,br) = score
        case ur@UnaryRule(_,_) =>
          unaryRules(rule.parent, ur) = score

      }
    }

    Grammar(features.labelIndex, features.ruleIndex, binaryRules, unaryRules)
  }
}
