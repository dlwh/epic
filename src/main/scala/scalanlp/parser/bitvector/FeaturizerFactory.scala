package scalanlp.parser
package bitvector

import scalala.tensor.counters.Counters.PairedDoubleCounter

trait FeaturizerFactory[L,W] {
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,W],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W];
}

class BitAugmentedFactory[L,W](normalFactory: FeaturizerFactory[L,W],
                               numStates: Int= 2,
                               suppressLoneRule: Boolean = false,
                               suppressLoneBit: Boolean = true) extends FeaturizerFactory[L,W] {
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,W],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W] = {
    val inner = normalFactory.getFeaturizer(baseLexicon,baseBinaries,baseUnaries);
    return new CrossProductFeaturizer(new AllPairsFeaturizer(new UnaryBitFeaturizer(numStates)),
                                      inner,suppressLoneBit,suppressLoneRule);
  }

}

class SlavAugmentedFactory[L,W](normalFactory: FeaturizerFactory[L,W],
                                numStates: Int = 2,
                                suppressLoneRule: Boolean = false,
                                suppressLoneBit: Boolean = true) extends FeaturizerFactory[L,W] {
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,W],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W] = {
    val inner = normalFactory.getFeaturizer(baseLexicon,baseBinaries,baseUnaries);
    return new CrossProductFeaturizer(new SlavBitFeaturizer,
                                      inner,suppressLoneBit,suppressLoneRule);
  }

}

class GenerativeFactory[L,W] extends FeaturizerFactory[L,W] {
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,W],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,W] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,W](baseBinaries, baseUnaries);
    val lex = new StupidGenerativeLexicalFeaturizer[L,W](baseLexicon);
    new SequenceFeaturizer(rules,lex);
  }
}

class WordShapeFactory[L] extends FeaturizerFactory[L,String] {
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,String],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,String] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,String](baseBinaries, baseUnaries);
    val lex = new WordShapeFeaturizer[L](baseLexicon);
    new SequenceFeaturizer(rules,lex);
  }
}


class WordClassFactory[L] extends FeaturizerFactory[L,String] {
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,String],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,String] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,String](baseBinaries, baseUnaries);
    val lex = new SlavUnknownWordFeaturizer[L](baseLexicon);
    new SequenceFeaturizer(rules,lex);
  }
}

class AllPairsWordShapeFactory[L] extends FeaturizerFactory[L,String] {
  def getFeaturizer(baseLexicon: PairedDoubleCounter[L,String],
                    baseBinaries: PairedDoubleCounter[L,BinaryRule[L]],
                    baseUnaries: PairedDoubleCounter[L,UnaryRule[L]]):Featurizer[L,String] = {
    val rules = new NormalGenerativeRuleFeaturizer[L,String](baseBinaries, baseUnaries);
    val lex = new AllPairsFeaturizer(new WordShapeFeaturizer[L](baseLexicon));
    new SequenceFeaturizer(rules,lex);
  }
}