package scalanlp.parser
package discrim

import scalanlp.config.Configuration
import scalala.tensor.Counter2


/**
 * 
 * @author dlwh
 */
trait FeaturizerFactory[L,W] {
  /** Returns a featurizer based on a configuration and a lexicon/production */
  def getFeaturizer(baseLexicon: Counter2[L,W,Double],
                    baseProductions: Counter2[L,BinaryRule[L],Double],
                    baseUnaries: Counter2[L,UnaryRule[L],Double]):Featurizer[L,W];
}

/**
 * Uses just rule features and wordshapes
 */
class PlainFeaturizerFactory[L](initToZero: Boolean = true) extends FeaturizerFactory[L,String] {
  def getFeaturizer(baseLexicon: Counter2[L,String,Double],
                    baseBinaries: Counter2[L,BinaryRule[L],Double],
                    baseUnaries: Counter2[L,UnaryRule[L],Double]):Featurizer[L,String] = {
    val lex = new WordShapeFeaturizer(baseLexicon,initToZero,1.0);
    val rules = new RuleFeaturizer[L,String](baseBinaries,baseUnaries,initToZero,1.0);

    new SumFeaturizer(rules,lex);
  }
}

/**
 * Uses just rule features, wordshapes, and the log probability of the rules
 */
class WeightedFeaturizerFactory[L](initToZero: Boolean = true) extends FeaturizerFactory[L,String] {
  def getFeaturizer(baseLexicon: Counter2[L,String,Double],
                    baseBinaries: Counter2[L,BinaryRule[L],Double],
                    baseUnaries: Counter2[L,UnaryRule[L],Double]):Featurizer[L,String] = {
    val lex = new WordShapeFeaturizer(baseLexicon,initToZero,1.0);
    val rules = new RuleFeaturizer[L,String](baseBinaries,baseUnaries,initToZero,1.0);
    val rules2 = new WeightedRuleFeaturizer[L,String](baseBinaries,baseUnaries,baseLexicon,initToZero,1.0);

    new SumFeaturizer(rules2,new SumFeaturizer(rules,lex));
  }
}

trait LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int):Featurizer[(L,Int),W];
}

class BitVectorFeaturizerFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new BitVectorFeaturizer(base,numStates);
}

class SlavLatentFeaturizerFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new SlavFeaturizer(base,numStates);
}

class SlavPlusLatentFeaturizerFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new SlavPlusFeaturizer(base,numStates);
}

class SlavSplitLatentFeaturizerFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new SlavSplitFeaturizer(base,numStates);
}

class HighLowFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new HighLowFeaturizer(base,numStates);
}
