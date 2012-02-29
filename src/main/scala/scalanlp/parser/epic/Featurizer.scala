package scalanlp.parser.epic

import scalala.tensor.Counter
import collection.mutable.ArrayBuffer
import scalanlp.parser.{UnaryRule, BinaryRule, Rule}

/**
 * A Featurizer turns decisions in a grammar into a set of features, possibly weighted
 * @author dlwh
 *
 * // TODO: make it about decisions so we can use with other kinds of models
 */
@SerialVersionUID(1)
trait Featurizer[L,W] extends Serializable {
  def featuresFor(r: Rule[L]):Counter[Feature, Double]
  def featuresFor(l: L, w: W):Counter[Feature, Double]

  /** should return 0.0 if we don't care about this feature. */
  def initialValueForFeature(f: Feature):Double
}


/**
 * Just returns features on the input rules
 */
class SimpleFeaturizer[L,W] extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = Counter(RuleFeature(r) -> 1.0)
  def featuresFor(l: L, w: W) = Counter(LexicalFeature(l,w) -> 1.0)

  def initialValueForFeature(f: Feature) = -1.0
}

/** Returns the sum of all features for two featurizers.  */
class SumFeaturizer[L,W](f1: Featurizer[L,W], f2: Featurizer[L,W]) extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = {
    val r1 = f1.featuresFor(r)
    val r2 = f2.featuresFor(r)
    if(r1.isEmpty) r2
    else if(r2.isEmpty) r1
    else  r1 + r2
  }
  def featuresFor(l: L, w: W)  = f1.featuresFor(l,w) + f2.featuresFor(l,w)

  def initialValueForFeature(f:  Feature) = f1.initialValueForFeature(f) + f2.initialValueForFeature(f)
}

/**
 * Takes another featurizer and wraps it in a SubstateFeature(baseFeature,<vector of parameters>)
 */
class SubstateFeaturizer[L,W](base: Featurizer[L,W]) extends Featurizer[(L,Int),W] {
  def featuresFor(r: Rule[(L,Int)]) = r match {
    case BinaryRule(a,b,c) =>
      val result = Counter[Feature,Double]()
      val baseFeatures = base.featuresFor(BinaryRule(a._1,b._1,c._1))
      val substates = ArrayBuffer(a._2, b._2, c._2)
      for( (k,v) <- baseFeatures.nonzero.pairs) {
        result(SubstateFeature(k,substates)) = v
      }
      result += baseFeatures
    case UnaryRule(a,b) =>
      val result = Counter[Feature,Double]()
      val baseFeatures = base.featuresFor(UnaryRule(a._1,b._1))
      val substates = ArrayBuffer(a._2,b._2)
      for( (k,v) <- baseFeatures.nonzero.pairs) {
        result(SubstateFeature(k,substates)) = v
      }
      result += baseFeatures
      result
  }

  def featuresFor(l: (L,Int), w: W) = {
    val baseFeatures = base.featuresFor(l._1, w)
    val substates = ArrayBuffer(l._2)
    val result = Counter[Feature,Double]()
    for( (k,v) <- baseFeatures.nonzero.pairs) {
      result(SubstateFeature(k,substates)) = v
    }
    result += baseFeatures
    result
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