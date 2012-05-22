package scalanlp.parser.features

import scalala.tensor.Counter
import scalanlp.trees.{UnaryRule, BinaryRule, Rule}
import scalanlp.epic.Feature
import collection.mutable.{ArrayBuilder, ArrayBuffer}

/**
 * A Featurizer turns decisions in a grammar into a set of features, possibly weighted
 *
 * This featurizes unanchored "simple annotated grammars," which is to say not
 * lexicalized and not span.
 *
 *
 * @author dlwh
 *
 */
// TODO: unify with other kinds of featurizers somehow.
@SerialVersionUID(1)
trait Featurizer[L, W] extends Serializable {
  def featuresFor(r: Rule[L]): Array[Feature]

  def featuresFor(l: L, w: W): Array[Feature]

  /**should return 0.0 if we don't care about this feature. */
  def initialValueForFeature(f: Feature): Double
}

/**
 * Just returns features on the input rules
 */
class SimpleFeaturizer[L,W] extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = Array(RuleFeature(r):Feature)
  def featuresFor(l: L, w: W) = Array(LexicalFeature(l,w):Feature)

  def initialValueForFeature(f: Feature) = -1.0
}

/** Returns the sum of all features for two featurizers.  */
class SumFeaturizer[L,W](f1: Featurizer[L,W], f2: Featurizer[L,W]) extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = {
    val r1 = f1.featuresFor(r)
    val r2 = f2.featuresFor(r)
    if(r1.isEmpty) r2
    else if(r2.isEmpty) r1
    else  r1 ++ r2
  }
  def featuresFor(l: L, w: W)  = f1.featuresFor(l,w) ++ f2.featuresFor(l,w)

  def initialValueForFeature(f:  Feature) = f1.initialValueForFeature(f) + f2.initialValueForFeature(f)
}

class GenFeaturizer[L,W](wGen: W=>IndexedSeq[Feature], lGen: L=>Seq[Feature] = {(x:L)=>Seq(IndicatorFeature(x))}) extends Featurizer[L,W] {
  def featuresFor(l: L, w: W) = {
    val result = ArrayBuilder.make[Feature]
    val wfFeats = wGen(w)
    val lfFeats = lGen(l)
    result.sizeHint(wfFeats.size * lfFeats.size)
    for ( wf <- wfFeats; lf <- lfFeats) {
      result +=  PairFeature(lf,wf)
    }
    result.result()
  }

  def featuresFor(r: Rule[L]) = r match {
    case BinaryRule(a,b,c) =>
      val result = ArrayBuilder.make[Feature]
      val af = lGen(a)
      val bf = lGen(b)
      val cf = lGen(c)
      result.sizeHint(af.size * bf.size * cf.size)
      for(aa <- af; bb <- bf; cc <- cf) {
        result += RuleFeature(BinaryRule(aa,bb,cc))
      }
      result.result()
    case UnaryRule(a,b) =>
      val result = ArrayBuilder.make[Feature]
      val af = lGen(a)
      val bf = lGen(b)
      result.sizeHint(af.size * bf.size)
      for(aa <- af; bb <- bf) {
        result += RuleFeature(UnaryRule(aa,bb))
      }
      result.result()

  }

  def initialValueForFeature(f: Feature) = 0.0
}

/**
 * Takes another featurizer and wraps it in a SubstateFeature(baseFeature,<vector of parameters>)
 */
class SubstateFeaturizer[L,W](base: Featurizer[L,W]) extends Featurizer[(L,Int),W] {
  def featuresFor(r: Rule[(L,Int)]) = r match {
    case BinaryRule(a,b,c) =>
      val result = ArrayBuilder.make[Feature]
      val baseFeatures = base.featuresFor(BinaryRule(a._1,b._1,c._1))
      result.sizeHint(baseFeatures.size * 2)
      val substates = ArrayBuffer(a._2, b._2, c._2)
      for( k <- baseFeatures) {
        result += SubstateFeature(k,substates)
      }
      result ++= baseFeatures
      result.result()
    case UnaryRule(a,b) =>
      val result = ArrayBuilder.make[Feature]
      val baseFeatures = base.featuresFor(UnaryRule(a._1,b._1))
      result.sizeHint(baseFeatures.size * 2)
      val substates = ArrayBuffer(a._2, b._2)
      for( k <- baseFeatures) {
        result += (SubstateFeature(k,substates))
      }
      result ++= baseFeatures
      result.result()
  }

  def featuresFor(l: (L,Int), w: W) = {
    val baseFeatures = base.featuresFor(l._1, w)
    val substates = ArrayBuffer(l._2)
    val result = ArrayBuilder.make[Feature]
    result.sizeHint(baseFeatures.size * 2)
    for( k <- baseFeatures) {
      result += (SubstateFeature(k,substates))
    }
    result ++= baseFeatures
    result.result()
  }

  def initialValueForFeature(f: Feature) = f match {
    case SubstateFeature(baseF, x) =>
      val baseScore = base.initialValueForFeature(baseF) //+ math.log(x.foldLeft(1.)(_ + 3 * _))
      assert(!baseScore.isNaN,baseF)
      //baseScore + math.log(1.0 - 1E-10 + math.random * 2 * 1E-10)
      val r = baseScore + math.log(1.0 - .1 + math.random * 2 * .1)
      assert(!r.isNaN,"post random: " + baseF)
      r
    case _ => base.initialValueForFeature(f)
  }
}