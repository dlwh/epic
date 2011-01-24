package scalanlp.parser
package discrim

import scalanlp.config.Configuration
import scalala.tensor.counters.Counters._


/**
 * 
 * @author dlwh
 */
trait FeaturizerFactory[L,W] {
  /** Returns a featurizer based on a configuration and a lexicon/production */
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,W],
                    baseProductions: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W];
}

/**
 * Uses just rule features and wordshapes
 */
class PlainFeaturizerFactory[L](initToZero: Boolean = true) extends FeaturizerFactory[L,String] {
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,String],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,String] = {
    val lex = new WordShapeFeaturizer(baseLexicon,initToZero,1.0);
    val rules = new RuleFeaturizer[L,String](baseBinaries,baseUnaries,initToZero,1.0);

    new SumFeaturizer(rules,lex);
  }
}

trait LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int):Featurizer[(L,Int),W];
}

class BitVectorFeaturizerFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new BitVectorFeaturizer(new CachingFeaturizer(base),numStates);
}

class SlavLatentFeaturizerFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new SlavFeaturizer(base,numStates);
}

class SlavPlusLatentFeaturizerFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new SlavPlusFeaturizer(new CachingFeaturizer(base),numStates);
}

class SlavSplitLatentFeaturizerFactory extends LatentFeaturizerFactory {
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new SlavSplitFeaturizer(new CachingFeaturizer(base),numStates);
}
