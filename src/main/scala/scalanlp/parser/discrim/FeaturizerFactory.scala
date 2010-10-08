package scalanlp.parser.discrim

import scalanlp.config.Configuration
import scalanlp.parser.Rule
import scalala.tensor.counters.Counters._;

/**
 * 
 * @author dlwh
 */
trait FeaturizerFactory[L,W] {
  /** Returns a featurizer based on a configuration and a lexicon/production */
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseProductions: PairedDoubleCounter[L,Rule[L]]):Featurizer[L,W];
}

/**
 * Uses just rule features and wordshapes
 */
class PlainFeaturizerFactory[L] extends FeaturizerFactory[L,String] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,String],
                    baseProductions: PairedDoubleCounter[L,Rule[L]]):Featurizer[L,String] = {
    val lex = new WordShapeFeaturizer(baseLexicon);
    val rules = new RuleFeaturizer[L,String](baseProductions);

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
  def getFeaturizer[L,W](base: Featurizer[L,W], numStates: Int) = new SlavFeaturizer(new CachingFeaturizer(base),numStates);
}

