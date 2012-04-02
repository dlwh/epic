package scalanlp.parser.epic

import scalala.tensor.dense._
import scalala.tensor.mutable.Counter2
import scalanlp.trees.{BinaryRule, UnaryRule}
import scalanlp.parser.{UnaryRule, BinaryRule, Grammar}

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
