package scalanlp.parser
package discrim

import scalala.tensor.mutable.Counter
import collection.mutable.ArrayBuffer

/**
 *
 * A Featurizer that returns features at each scale in the feature cascade
 *
 * @author dlwh
 */
class MultiscaleFeaturizer[L,W](base: Featurizer[L,W]) extends Featurizer[(L,Seq[Int]),W] {
  def featuresFor(r: Rule[(L,Seq[Int])]) = r match {
    case BinaryRule(a,b,c) =>
      val result = Counter[Feature[(L,Seq[Int]),W],Double]()
      val baseFeatures = base.featuresFor(BinaryRule(a._1,b._1,c._1))
      for( (k,v) <- baseFeatures.nonzero.pairs) {
        var i = 0;
        while(i < a._2.length) {
          val substates = ArrayBuffer(a._2.drop(i),b._2.drop(i),c._2.drop(i))
          result(SubstateFeature(k,substates)) = v
          i += 1
        }
      }
      result
    case UnaryRule(a,b) =>
      val result = Counter[Feature[(L,Seq[Int]),W],Double]()
      val baseFeatures = base.featuresFor(UnaryRule(a._1,b._1))
      for( (k,v) <- baseFeatures.nonzero.pairs) {
        var i = 0;
        while(i < a._2.length) {
          val substates = ArrayBuffer(a._2.drop(i),b._2.drop(i))
          result(SubstateFeature(k,substates)) = v
          i += 1
        }
      }
      result
  }

  def featuresFor(l: (L,Seq[Int]), w: W) = {
    val baseFeatures = base.featuresFor(l._1, w)
    val result = Counter[Feature[(L,Seq[Int]),W],Double]()
    for( (k,v) <- baseFeatures.nonzero.pairs) {
      var i = 0;
      while(i < l._2.length) {
        val substates = ArrayBuffer(l._2.drop(i))
        result(SubstateFeature(k,substates)) = v
        i += 1
      }
    }
    result
  }

  def initialValueForFeature(f: Feature[(L,Seq[Int]),W]) = f match {
    case SubstateFeature(baseF, x) =>
        println(baseF)
        val baseScore = if(x(0).length ==0) 0.0 else base.initialValueForFeature(baseF)
        assert(!baseScore.isNaN,baseF)
        baseScore + math.log(1.0 - .1 + math.random * 2 * .1)
    case f => println(f);0.0
  }
}