package scalanlp.parser.discrim

import scalala.tensor.Counter2
import scalanlp.parser.{Rule, AnnotatedLabel, UnaryRule, BinaryRule}
import scalala.tensor.Counter
import scalala.library.Library._

/**
 * 
 * @author dlwh
 */

/**
 * Just returns indicators on rule features, no lexical features
 */
class AnnotatedLabelFeaturizer[W](binaries: Counter2[AnnotatedLabel,BinaryRule[AnnotatedLabel], Double],
                          unaries: Counter2[AnnotatedLabel,UnaryRule[AnnotatedLabel],Double],
                          initToZero: Boolean = true, scale: Double = 1.0) extends Featurizer[AnnotatedLabel,W] {
  def featuresFor(r: Rule[AnnotatedLabel]) = Counter(RuleFeature(r) -> 1.0)
  def featuresFor(l: AnnotatedLabel, w: W) = Counter[Feature[AnnotatedLabel,W],Double]()
  val unaryTotals = sum(unaries,Axis.Vertical)
  val binaryTotals = sum(binaries,Axis.Vertical)

  def initialValueForFeature(f: Feature[AnnotatedLabel,W]) = f match {
    case RuleFeature(r:BinaryRule[AnnotatedLabel]) => if(initToZero) 0.0  else{
      val s = math.log(binaries(r.parent,r) / binaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite) 0.0 else s
    }
    case RuleFeature(r:UnaryRule[AnnotatedLabel]) => if(initToZero) 0.0 else {
      val s = math.log(unaries(r.parent,r) / unaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite)  0.0 else s
    }
    case _ => 0.0
  }

}


/**
 * Uses just rule features and wordshapes
 */
class PlainFeaturizerFactory[L](initToZero: Boolean = true) extends FeaturizerFactory[L,String] {
  def getFeaturizer(baseLexicon: Counter2[L,String,Double],
                    baseBinaries: Counter2[L,BinaryRule[L],Double],
                    baseUnaries: Counter2[L,UnaryRule[L],Double]):Featurizer[L,String] = {
    val lex = new WordShapeFeaturizer(baseLexicon,initToZero,1.0);
    val rules = new RuleFeaturizer[L,String](baseBinaries,baseUnaries,initToZero,1.0);

    new SumFeaturizer(rules,lex);
  }
}