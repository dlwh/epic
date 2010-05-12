package scalanlp.parser
package bitvector

import scalala.tensor.counters.Counters.PairedDoubleCounter;

import LogisticBitVector._;import scalanlp.config.Configuration


case class SlavRuleFeature[L,W](r: Rule[(L,Int)]) extends Feature[L,W];
case class SlavLexicalFeature[L,W](parent: L, state: Int, word: W) extends Feature[L,W]

class SlavFeaturizer[L,W](baseLexicon: PairedDoubleCounter[L,W],
                          baseProductions: PairedDoubleCounter[L,Rule[L]], numBits: Int) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = d match {
    case WordDecision(w) =>
      Seq(SlavLexicalFeature(c._1,c._2,w))
    case UnaryRuleDecision(child,state) =>
      val rule = UnaryRule(c,(child,state));
      val ruleFeature = SlavRuleFeature[L,W](rule);
      Seq(ruleFeature)
    case BinaryRuleDecision(left,lstate,right,rstate) =>
      val rule = BinaryRule(c,(left,lstate),(right,rstate));
      val ruleFeature = SlavRuleFeature[L,W](rule);
      Seq(ruleFeature)
  }
  def priorForFeature(f: LogisticBitVector.Feature[L,W]) = {
    f match {
      case SlavRuleFeature(BinaryRule((par,parstate),(lchild,lchildState),(rchild,rchildState))) =>
        val baseRule = BinaryRule(par,lchild,rchild);
        Some(Math.log(baseProductions(par, baseRule)));
      case SlavRuleFeature(UnaryRule((par,parstate),(child,childState))) =>
        val baseRule = UnaryRule(par,child);
        Some(Math.log(baseProductions(par, baseRule)));
      case SlavLexicalFeature(parent,_, word) => Some(Math.log(baseLexicon(parent,word)));
      case _ => None
    }
  }
}

class SlavFeatureFactory[L,W] extends FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseProductions: PairedDoubleCounter[L,Rule[L]]):Featurizer[L,W] = {
    val numBits = conf.readIn[Int]("numBits");
    new SlavFeaturizer(baseLexicon,baseProductions,numBits);
  }
}