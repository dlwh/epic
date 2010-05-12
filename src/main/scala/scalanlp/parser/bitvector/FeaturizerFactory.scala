package scalanlp.parser
package bitvector

import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalanlp.config.Configuration

trait FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseProductions: PairedDoubleCounter[L,Rule[L]]):Featurizer[L,W];
}

class BitAugmentedFactory[L,W](normalFactory: FeaturizerFactory[L,W]) extends FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseProductions: PairedDoubleCounter[L,Rule[L]]):Featurizer[L,W] = {
    val numBits = conf.readIn[Int]("numBits");
    val inner = normalFactory.getFeaturizer(conf,baseLexicon,baseProductions);
    val suppressLoneRule = conf.readIn[Boolean]("featurizer.suppressLoneRule");
    val suppressLoneBit = conf.readIn[Boolean]("featurizer.suppressLoneBit");
    return new CrossProductFeaturizer(new AllPairsFeaturizer(new UnaryBitFeaturizer(numBits)),
                                      inner,suppressLoneBit,suppressLoneRule);
  }

}

class GenerativeFactory[L,W] extends FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseProductions: PairedDoubleCounter[L,Rule[L]]):Featurizer[L,W] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,W](baseProductions);
    val lex = new StupidGenerativeLexicalFeaturizer[L,W](baseLexicon);
    new SequenceFeaturizer(rules,lex);
  }
}

class WordShapeFactory[L] extends FeaturizerFactory[L,String] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,String],
                    baseProductions: PairedDoubleCounter[L,Rule[L]]):Featurizer[L,String] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,String](baseProductions);
    val lex = new WordShapeFeaturizer[L](baseLexicon);
    new SequenceFeaturizer(rules,lex);
  }
}

