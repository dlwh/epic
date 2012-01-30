package scalanlp.parser.discrim

import scalala.tensor.Counter2
import scalanlp.parser.{Rule, AnnotatedLabel, UnaryRule, BinaryRule}
import scalala.tensor.Counter
import scalala.library.Library._
import collection.immutable.Set
import scalanlp.parser.Annotation

/**
 * 
 * @author dlwh
 */

class AnnotatedLabelFeaturizer(binaries: Counter2[AnnotatedLabel,BinaryRule[AnnotatedLabel], Double],
                          unaries: Counter2[AnnotatedLabel,UnaryRule[AnnotatedLabel],Double],
                          wordFeaturizer: Featurizer[AnnotatedLabel,String],
                          initToZero: Boolean = false, scale: Double = 1.0) extends Featurizer[AnnotatedLabel,String] {

  val allSymbols: Set[Annotation] = {
    Set.empty ++ {
      for(lbl <- (binaries.keysIterator.map(_._1) ++ unaries.keysIterator.map(_._1));
          sym <- lbl.features) yield sym
    }
  }

  val labelTransforms:Seq[AnnotatedLabel=>AnnotatedLabel] = {
    Seq (
    identity[AnnotatedLabel] _ ,
    {(_:AnnotatedLabel).copy(parents=Seq.empty,siblings=Seq.empty,features=Set.empty)},
    {(_:AnnotatedLabel).copy(parents=Seq.empty,features=Set.empty)},
    {(_:AnnotatedLabel).copy(siblings=Seq.empty,features=Set.empty)},
    {(_:AnnotatedLabel).copy(parents=Seq.empty,siblings=Seq.empty)}
    ) ++ {
      // try each symbol by itself
      for(sym <- allSymbols.iterator) yield {
        val set = Set(sym);
        {(x:AnnotatedLabel) =>
          x.copy(parents=Seq.empty,
            siblings=Seq.empty,
            features = if(x.features(sym)) set else Set.empty)
        }
      }
    }
  }

  def featuresFor(r: Rule[AnnotatedLabel]) = {
    val ctr = Counter[Feature[AnnotatedLabel,String],Double]()
    for(transform <- labelTransforms) {
      ctr(RuleFeature(r.map(transform))) = 1.0
    }
    ctr
  }

  def featuresFor(l: AnnotatedLabel, w: String) = {
    val base = wordFeaturizer.featuresFor(l,w)
    // TODO: augment
    base
  }
  val unaryTotals = sum(unaries,Axis.Vertical)
  val binaryTotals = sum(binaries,Axis.Vertical)

  def initialValueForFeature(f: Feature[AnnotatedLabel,String]) = f match {
    case RuleFeature(r:BinaryRule[AnnotatedLabel]) => if(initToZero) 0.0  else{
      val s = math.log(binaries(r.parent,r) / binaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite) 0.0 else s
    }
    case RuleFeature(r:UnaryRule[AnnotatedLabel]) => if(initToZero) 0.0 else {
      val s = math.log(unaries(r.parent,r) / unaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite)  0.0 else s
    }
    case _ => wordFeaturizer.initialValueForFeature(f)
  }

}


/**
 * Uses just rule features and wordshapes
 */
class AnnotatedLabelFeatureFactory(initToZero: Boolean = true) extends FeaturizerFactory[AnnotatedLabel,String] {
  def getFeaturizer(baseLexicon: Counter2[AnnotatedLabel,String,Double],
                    baseBinaries: Counter2[AnnotatedLabel,BinaryRule[AnnotatedLabel],Double],
                    baseUnaries: Counter2[AnnotatedLabel,UnaryRule[AnnotatedLabel],Double]):Featurizer[AnnotatedLabel,String] = {
    val lex = new WordShapeFeaturizer(baseLexicon,initToZero,1.0);
    val rules = new AnnotatedLabelFeaturizer(baseBinaries,baseUnaries,lex, initToZero,1.0);
    rules
  }
}

/**
 *
 * @author dlwh
 */

class VarMarkovFeaturizer(binaries: Counter2[AnnotatedLabel,BinaryRule[AnnotatedLabel], Double],
                          unaries: Counter2[AnnotatedLabel,UnaryRule[AnnotatedLabel],Double],
                          wordFeaturizer: Featurizer[AnnotatedLabel,String],
                          initToZero: Boolean = false, scale: Double = 1.0) extends Featurizer[AnnotatedLabel,String] {

  val allSymbols: Set[Annotation] = {
    Set.empty ++ {
      for(lbl <- (binaries.keysIterator.map(_._1) ++ unaries.keysIterator.map(_._1));
          sym <- lbl.features) yield sym
    }
  }

  val labelTransforms:Seq[AnnotatedLabel=>AnnotatedLabel] = {
    Seq (
//    identity[AnnotatedLabel] _ ,
//    {(_:AnnotatedLabel).copy(parents=Seq.empty,siblings=Seq.empty,features=Set.empty)},
    {(a:AnnotatedLabel) => a.copy(parents=a.parents.take(1),siblings=a.siblings.take(1))},
    {(a:AnnotatedLabel) => a.copy(siblings=Seq.empty)},
    {(a:AnnotatedLabel) => a.copy(parents=Seq.empty)}
    ) ++ {
      // try each symbol by itself
      for(sym <- allSymbols.iterator) yield {
        val set = Set(sym);
        {(x:AnnotatedLabel) =>
          x.copy(parents=Seq.empty,
            siblings=Seq.empty,
            features = if(x.features(sym)) set else Set.empty)
        }
      }
    }
  }

  def featuresFor(r: Rule[AnnotatedLabel]) = {
    val ctr = Counter[Feature[AnnotatedLabel,String],Double]()
    for(transform <- labelTransforms) {
      ctr(RuleFeature(r.map(transform))) = 1.0
    }
    ctr
  }

  def featuresFor(l: AnnotatedLabel, w: String) = {
    val base = wordFeaturizer.featuresFor(l,w)
    // TODO: augment
    base
  }
  val unaryTotals = sum(unaries,Axis.Vertical)
  val binaryTotals = sum(binaries,Axis.Vertical)

  def initialValueForFeature(f: Feature[AnnotatedLabel,String]) = f match {
    case RuleFeature(r:BinaryRule[AnnotatedLabel]) => if(initToZero) 0.0  else{
      val s = math.log(binaries(r.parent,r) / binaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite) 0.0 else s
    }
    case RuleFeature(r:UnaryRule[AnnotatedLabel]) => if(initToZero) 0.0 else {
      val s = math.log(unaries(r.parent,r) / unaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite)  0.0 else s
    }
    case _ => wordFeaturizer.initialValueForFeature(f)
  }

}

/**
 * Uses just rule features and wordshapes
 */
class VarMarkovFeaturizerFactory(initToZero: Boolean = true) extends FeaturizerFactory[AnnotatedLabel,String] {
  def getFeaturizer(baseLexicon: Counter2[AnnotatedLabel,String,Double],
                    baseBinaries: Counter2[AnnotatedLabel,BinaryRule[AnnotatedLabel],Double],
                    baseUnaries: Counter2[AnnotatedLabel,UnaryRule[AnnotatedLabel],Double]):Featurizer[AnnotatedLabel,String] = {
    val lex = new WordShapeFeaturizer(baseLexicon,initToZero,1.0);
    val rules = new VarMarkovFeaturizer(baseBinaries,baseUnaries,lex, initToZero,1.0);
    rules
  }
}