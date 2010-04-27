package scalanlp.parser.bitvector

import scalanlp.parser._
import LogisticBitVector._;
import scalanlp.util.CachedHashCode


trait Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W], c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]];
  def initFeatureWeight(logit: LogisticBitVector[L,W], f: LogisticBitVector.Feature[L,W]):Option[Double]
}

class NormalGenerativeFeaturizer[L,W] extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W], c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = d match {
    case WordDecision(w) =>
      Seq(LexicalFeature(c._1,w))
    case UnaryRuleDecision(child,state) =>
      val rule = UnaryRule(c._1,child);
      val ruleFeature = RuleFeature[L,W](rule);
      Seq(ruleFeature)
    case BinaryRuleDecision(left,lstate,right,rstate) =>
      val rule = BinaryRule(c._1,left,right);
      val ruleFeature = RuleFeature[L,W](rule);
      Seq(ruleFeature)
  }
  def initFeatureWeight(logit: LogisticBitVector[L,W], f: LogisticBitVector.Feature[L,W]) = f match {
    case RuleFeature(r:Rule[L]) => Some(Math.log(logit.initProductions(r.parent, r)));
    case LexicalFeature(parent:L, word:W) => Some(Math.log(logit.initLexicon(parent,word)));
    case _ => None
  }
}
class UnaryBitFeaturizer[L,W](numBits:Int) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W], c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = d match {
    case WordDecision(w) =>
      Seq(LexicalFeature(c._1,w))
    case UnaryRuleDecision(child,state) =>
      val childFeatures = mkBitStati(numBits, UChild,state);
      val parentFeatures = mkBitStati(numBits, Parent,c._2);
      childFeatures ++ parentFeatures toSeq
    case BinaryRuleDecision(left,lstate,right,rstate) =>
      val lchildFeatures = mkBitStati(numBits, LChild,lstate);
      val rchildFeatures = mkBitStati(numBits, RChild,rstate);
      val parentFeatures = mkBitStati(numBits, Parent,c._2);
      parentFeatures ++ lchildFeatures ++ rchildFeatures toSeq
  }
  def initFeatureWeight(logit: LogisticBitVector[L,W], f: LogisticBitVector.Feature[L,W]) = f match {
    case SingleBitFeature(_,_,_) => Some(Math.log(Math.random * 0.2 + 0.9))
    case _ => None
  } }

class PairFeaturizer[L,W](featurizers: Seq[Featurizer[L,W]]) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W], c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = {
    val allFeatures = (for( feat <- featurizers; f <- feat.features(d,c)) yield f).toIndexedSeq;
    val unionFeatures = for( i <- 0 until allFeatures.length; j <- (i+1) until allFeatures.length)
      yield UnionFeature(Seq(allFeatures(i),allFeatures(j)))
    allFeatures ++ unionFeatures;
  }
  def initFeatureWeight(logit: LogisticBitVector[L,W], f: LogisticBitVector.Feature[L,W]) = f match {
    case UnionFeature(feats) => Some(feats.map{scoreFeature(logit,_)}.foldLeft(0.0){ _ + _.get })
    case f => scoreFeature(logit,f);
  }

  private def scoreFeature(logit: LogisticBitVector[L,W], f: Feature[L,W]) = {
    featurizers.iterator.map{_.initFeatureWeight(logit,f)}.find(_ != None).getOrElse(None);
  }
}

class StandardFeaturizer[L,W](numBits: Int) extends PairFeaturizer[L,W](Seq(new UnaryBitFeaturizer[L,W](numBits),new NormalGenerativeFeaturizer[L,W])) with Featurizer[L,W];

case class SlavRuleFeature[L,W](r: Rule[(L,Int)]) extends Feature[L,W] with CachedHashCode;
case class SlavLexicalFeature[L,W](parent: L, state: Int, word: W) extends Feature[L,W] with CachedHashCode;

class SlavFeaturizer[L,W](numBits: Int) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W], c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = d match {
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
  def initFeatureWeight(logit: LogisticBitVector[L,W], f: LogisticBitVector.Feature[L,W]) = {
    val noise = 0.2 * Math.random + 0.9
    f match {
      case SlavRuleFeature(BinaryRule((par,parstate),(lchild,lchildState),(rchild,rchildState))) =>
        val baseRule = BinaryRule(par,lchild,rchild);
        Some(Math.log(logit.initProductions(par, baseRule) * noise));
      case SlavRuleFeature(UnaryRule((par,parstate),(child,childState))) =>
        val baseRule = UnaryRule(par,child);
        Some(Math.log(logit.initProductions(par, baseRule) * noise));
      case SlavLexicalFeature(parent,_, word) => Some(Math.log(logit.initLexicon(parent,word) * noise));
      case _ => None
    }
  }
}