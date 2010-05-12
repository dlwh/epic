package scalanlp.parser.bitvector

import scala.collection.mutable.ArrayBuffer
import scalala.tensor.counters.Counters.PairedDoubleCounter;
import scalanlp.parser._
import scalanlp.util.CachedHashCode
import LogisticBitVector._;

trait Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]];
  def initFeatureWeight(f: LogisticBitVector.Feature[L,W]):Option[Double]
}

class NormalGenerativeRuleFeaturizer[L,W](baseProductions: PairedDoubleCounter[L,Rule[L]]) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = d match {
    /*case WordDecision(w) =>
      ArrayBuffer(LexicalFeature(c._1,w))
    */
    case UnaryRuleDecision(child,state) =>
      val rule = UnaryRule(c._1,child);
      val ruleFeature = RuleFeature[L](rule);
      Seq(ruleFeature)
    case BinaryRuleDecision(left,lstate,right,rstate) =>
      val rule = BinaryRule(c._1,left,right);
      val ruleFeature = RuleFeature[L](rule);
      Seq(ruleFeature)
    case _ => Seq.empty;
  }
  def initFeatureWeight(f: LogisticBitVector.Feature[L,W]) = f match {
    case RuleFeature(r:Rule[L]) => Some(Math.log(baseProductions(r.parent, r)));
    case _ => None
  }
}

class StupidGenerativeLexicalFeaturizer[L,W](baseLexicon: PairedDoubleCounter[L,W]) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = d match {
    case WordDecision(w) =>
      ArrayBuffer(LexicalFeature(c._1,w))
    case _ => Seq.empty
  }

  def initFeatureWeight(f: LogisticBitVector.Feature[L,W]) = f match {
    case LexicalFeature(parent:L, word:W) => Some(Math.log(baseLexicon(parent,word)));
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

  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = d match {
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
  def initFeatureWeight(f: LogisticBitVector.Feature[L,W]) = f match {
    case SingleBitFeature(_,_,_) => Some(Math.log(Math.random * 0.2 + 0.9))
    case _ => None
  }
}

class CrossProductFeaturizer[L,W](f1: Featurizer[L,W], f2: Featurizer[L,W],
                                  suppressLeftFeatures: Boolean=false,
                                  suppressRightFeatures: Boolean = false) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = {
    val feats1 = f1.features(d,c);
    val feats2 = f2.features(d,c);
    val unionFeatures = for(ff1 <- feats1; ff2 <- feats2)
      yield UnionFeature(ff1,ff2);
    var rv:Seq[LogisticBitVector.Feature[L,W]] = unionFeatures;
    if(!suppressRightFeatures) rv = rv ++ feats2;
    if(!suppressLeftFeatures) rv = rv ++ feats1;
    rv;
  }

  def initFeatureWeight(f: LogisticBitVector.Feature[L,W]) = f match {
    case UnionFeature(f1,f2) => Some(scoreFeature(f1).get + scoreFeature(f2).get);
    case f => scoreFeature(f);
  }

  private def scoreFeature(f: Feature[L,W]) = {
    Iterator(f1,f2).map{_.initFeatureWeight(f)}.find(_ != None).getOrElse(None);
  }
}

class SequenceFeaturizer[L,W](inner: Featurizer[L,W]*) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = {
    val allFeatures = for(feat <- inner; f <- feat.features(d,c)) yield f;
    allFeatures;
  }
  def initFeatureWeight(f: LogisticBitVector.Feature[L,W]) = f match {
    case f => scoreFeature(f);
  }

  private def scoreFeature(f: Feature[L,W]) = {
    inner.iterator.map{_.initFeatureWeight(f)}.find(_ != None).getOrElse(None);
  }
}

class AllPairsFeaturizer[L,W](inner: Featurizer[L,W]) extends Featurizer[L,W] {
  def features(d: LogisticBitVector.Decision[L,W],
               c: LogisticBitVector.Context[L]): Seq[LogisticBitVector.Feature[L,W]] = {
    val allFeatures = (for( f <- inner.features(d,c).view) yield f).toIndexedSeq;
    val unionFeatures = for( i <- Iterator.range(0,allFeatures.length); j <- Iterator.range((i+1),allFeatures.length))
      yield UnionFeature(allFeatures(i),allFeatures(j))
    allFeatures ++ unionFeatures;
  }
  def initFeatureWeight(f: LogisticBitVector.Feature[L,W]) = f match {
    case UnionFeature(f1,f2) => Some(scoreFeature(f1).get +
                                     scoreFeature(f2).get);
    case f => scoreFeature(f);
  }

  private def scoreFeature(f: Feature[L,W]) = {
    inner.initFeatureWeight(f);
  }
}


