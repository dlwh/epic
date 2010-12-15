package scalanlp.parser
package bitvector

import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalanlp.config.Configuration

trait FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W];
}

class BitAugmentedFactory[L,W](normalFactory: FeaturizerFactory[L,W]) extends FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W] = {
    val numStates = conf.readIn[Int]("numStates");
    val inner = normalFactory.getFeaturizer(conf,baseLexicon,baseBinaries,baseUnaries);
    val suppressLoneRule = conf.readIn[Boolean]("featurizer.suppressLoneRule");
    val suppressLoneBit = conf.readIn[Boolean]("featurizer.suppressLoneBit");
    return new CrossProductFeaturizer(new AllPairsFeaturizer(new UnaryBitFeaturizer(numStates)),
                                      inner,suppressLoneBit,suppressLoneRule);
  }

}

class SlavAugmentedFactory[L,W](normalFactory: FeaturizerFactory[L,W]) extends FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W] = {
    val numStates = conf.readIn[Int]("numStates");
    val inner = normalFactory.getFeaturizer(conf,baseLexicon,baseBinaries,baseUnaries);
    val suppressLoneRule = conf.readIn[Boolean]("featurizer.suppressLoneRule",true);
    val suppressLoneBit = conf.readIn[Boolean]("featurizer.suppressLoneBit",true);
    return new CrossProductFeaturizer(new SlavBitFeaturizer,
                                      inner,suppressLoneBit,suppressLoneRule);
  }

}

class GenerativeFactory[L,W] extends FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,W](baseBinaries, baseUnaries);
    val lex = new StupidGenerativeLexicalFeaturizer[L,W](baseLexicon);
    new SequenceFeaturizer(rules,lex);
  }
}

class WordShapeFactory[L] extends FeaturizerFactory[L,String] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,String],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,String] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,String](baseBinaries, baseUnaries);
    val lex = new WordShapeFeaturizer[L](baseLexicon);
    new SequenceFeaturizer(rules,lex);
  }
}


class WordClassFactory[L] extends FeaturizerFactory[L,String] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,String],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,String] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,String](baseBinaries, baseUnaries);
    val lex = new SlavUnknownWordFeaturizer[L](baseLexicon);
    new SequenceFeaturizer(rules,lex);
  }
}

class AllPairsWordShapeFactory[L] extends FeaturizerFactory[L,String] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,String],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,String] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,String](baseBinaries, baseUnaries);
    val lex = new AllPairsFeaturizer(new WordShapeFeaturizer[L](baseLexicon));
    new SequenceFeaturizer(rules,lex);
  }
}