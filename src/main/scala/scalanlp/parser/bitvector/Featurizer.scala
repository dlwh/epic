package scalanlp.parser.bitvector

import scala.collection.mutable.ArrayBuffer
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
      ArrayBuffer(LexicalFeature(c._1,w))
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
  def mkBitStati(numBits: Int, lbl: LabelOfBit, state: Substate) = {
    for( (bit,toggled) <- BitUtils.iterateBits(state,numBits)) yield {
      stati(lbl.index)(bit)(toggled);
    }
  } toSeq;

  private val stati = Array.tabulate(4,numBits,2) { (parentId, bit, toggled) =>
    SingleBitFeature(LogisticBitVector.bitLabels(parentId),bit,toggled);
  }

  def features(d: LogisticBitVector.Decision[L,W], c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = d match {
    case WordDecision(w) =>
      val parentFeatures = mkBitStati(numBits, Parent,c._2);
      parentFeatures toSeq
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
  }
}

class CrossProductFeaturizer[L,W](f1: Featurizer[L,W], f2: Featurizer[L,W],
                                  suppressLeftFeatures: Boolean=false,
                                  suppressRightFeatures: Boolean = false) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W], c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = {
    val feats1 = f1.features(d,c);
    val feats2 = f2.features(d,c);
    val unionFeatures = for(ff1 <- feats1; ff2 <- feats2)
      yield UnionFeature(ff1,ff2);
    var rv:Seq[LogisticBitVector.Feature[L,W]] = unionFeatures;
    if(!suppressRightFeatures) rv = rv ++ feats2;
    if(!suppressLeftFeatures) rv = rv ++ feats1;
    rv;
  }
  def initFeatureWeight(logit: LogisticBitVector[L,W], f: LogisticBitVector.Feature[L,W]) = f match {
    case UnionFeature(f1,f2) => Some(scoreFeature(logit,f1).get + scoreFeature(logit,f2).get);
    case f => scoreFeature(logit,f);
  }

  private def scoreFeature(logit: LogisticBitVector[L,W], f: Feature[L,W]) = {
    Iterator(f1,f2).map{_.initFeatureWeight(logit,f)}.find(_ != None).getOrElse(None);
  }
}

class AllPairsFeaturizer[L,W](inner: Featurizer[L,W]) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W], c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = {
    val allFeatures = (for( f <- inner.features(d,c).view) yield f).toIndexedSeq;
    val unionFeatures = for( i <- Iterator.range(0,allFeatures.length); j <- Iterator.range((i+1),allFeatures.length))
      yield UnionFeature(allFeatures(i),allFeatures(j))
    allFeatures ++ unionFeatures;
  }
  def initFeatureWeight(logit: LogisticBitVector[L,W], f: LogisticBitVector.Feature[L,W]) = f match {
    case UnionFeature(f1,f2) => Some(scoreFeature(logit,f1).get + scoreFeature(logit,f2).get);
    case f => scoreFeature(logit,f);
  }

  private def scoreFeature(logit: LogisticBitVector[L,W], f: Feature[L,W]) = {
    inner.initFeatureWeight(logit,f);
  }
}

class StandardFeaturizer[L,W](numBits: Int,suppressLoneBit: Boolean, suppressLoneRule: Boolean) extends CrossProductFeaturizer[L,W](
  new AllPairsFeaturizer(new UnaryBitFeaturizer[L,W](numBits)),
  new NormalGenerativeFeaturizer[L,W],suppressLoneBit,suppressLoneRule) with Featurizer[L,W];

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