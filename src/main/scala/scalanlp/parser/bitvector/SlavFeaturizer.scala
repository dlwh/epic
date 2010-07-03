package scalanlp.parser
package bitvector

import scalala.tensor.counters.Counters.PairedDoubleCounter;

import LogisticBitVector._;
import scalanlp.config.Configuration


case class SlavRuleFeature[L,W](r: Rule[(L,Int)]) extends Feature[L,W];
case class SlavLexicalFeature[L,W](parent: L, state: Int, word: W) extends Feature[L,W]

class SlavFeaturizer[L,W](baseLexicon: PairedDoubleCounter[L,W],
                          baseProductions: PairedDoubleCounter[L,Rule[L]]) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): IndexedSeq[LogisticBitVector.Feature[L,W]] = d match {
    case WordDecision(w) =>
      IndexedSeq(SlavLexicalFeature(c._1,c._2,w))
    case UnaryRuleDecision(child,state) =>
      val rule = UnaryRule(c,(child,state));
      val ruleFeature = SlavRuleFeature[L,W](rule);
      IndexedSeq(ruleFeature)
    case BinaryRuleDecision(left,lstate,right,rstate) =>
      val rule = BinaryRule(c,(left,lstate),(right,rstate));
      val ruleFeature = SlavRuleFeature[L,W](rule);
      IndexedSeq(ruleFeature)
  }
  def initialValueForFeature(f: LogisticBitVector.Feature[L,W]) = {
    f match {
      case SlavRuleFeature(BinaryRule((par,parstate),(lchild,lchildState),(rchild,rchildState))) =>
        val baseRule = BinaryRule(par,lchild,rchild);
        if( baseProductions(par, baseRule) == 0) Some(Double.NegativeInfinity)
        else Some(math.log(baseProductions(par,baseRule)) - math.log(baseProductions(par).total));
      case SlavRuleFeature(UnaryRule((par,parstate),(child,childState))) =>
        val baseRule = UnaryRule(par,child);
        if( baseProductions(par, baseRule) == 0) Some(Double.NegativeInfinity)
        else Some(math.log(baseProductions(par,baseRule)) - math.log(baseProductions(par).total));
      case SlavLexicalFeature(parent,_, word) => Some(math.log(baseLexicon(parent,word)));
        if( baseLexicon(parent, word) == 0) Some(Double.NegativeInfinity)
        else Some(math.log(baseLexicon(parent,word)) - math.log(baseLexicon(parent).total));
      case _ => None
    }
  }

    def priorForFeature(f: LogisticBitVector.Feature[L,W]) = {
    f match {
      case SlavRuleFeature(BinaryRule((par,parstate),(lchild,lchildState),(rchild,rchildState))) =>
        Some(0.0);
      case SlavRuleFeature(UnaryRule((par,parstate),(child,childState))) =>
        Some(0.0);
      case SlavLexicalFeature(parent,_, word) => Some(math.log(baseLexicon(parent,word)));
        Some(0.0);
      case _ => None
    }
  }
}

case class SlavUnarySubstateFeature(parState: Int, childState: Int) extends Feature[Nothing,Nothing];
case class SlavBinarySubstateFeature(parState: Int, lchildState: Int, rchildState: Int) extends Feature[Nothing,Nothing];
case class SlavLexicalSubstateFeature(parState: Int) extends Feature[Nothing,Nothing];

class SlavBitFeaturizer[L,W] extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): IndexedSeq[LogisticBitVector.Feature[L,W]] = d match {
    case WordDecision(w) =>
      IndexedSeq(SlavLexicalSubstateFeature(c._2));
    case UnaryRuleDecision(child,state) =>
      IndexedSeq(SlavUnarySubstateFeature(c._2,state));
    case BinaryRuleDecision(left,lstate,right,rstate) =>
      IndexedSeq(SlavBinarySubstateFeature(c._2,lstate,rstate));
  }
  def priorForFeature(f: LogisticBitVector.Feature[L,W]) = {
    f match {
      case _ : SlavLexicalSubstateFeature => Some(0.0);
      case _ : SlavBinarySubstateFeature => Some(0.0);
      case _ : SlavUnarySubstateFeature => Some(0.0);
      case _ => None
    }
  }

  def initialValueForFeature(f: LogisticBitVector.Feature[L,W]) = {
    f match {
      case _ : SlavLexicalSubstateFeature => Some(0.0);
      case _ : SlavBinarySubstateFeature => Some(0.0);
      case _ : SlavUnarySubstateFeature => Some(0.0);
      case _ => None
    }
  }
}

class SlavFeatureFactory[L,W] extends FeaturizerFactory[L,W] {
  def getFeaturizer(conf: Configuration,
                    baseLexicon: PairedDoubleCounter[L,W],
                    baseProductions: PairedDoubleCounter[L,Rule[L]]):Featurizer[L,W] = {
    new SlavFeaturizer(baseLexicon,baseProductions);
  }
}