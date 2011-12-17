package scalanlp.parser
package discrim

import scalanlp.util.{CachedHashCode, Encoder, Index}
import collection.mutable.ArrayBuffer

import projections._
import scalanlp.trees._
import scalala.tensor.{Counter2, Counter}
import scalala.library.Library._
import scalala.tensor.sparse.SparseVector
import scalanlp.collection.mutable.{OpenAddressHashArray, SparseArrayMap, ArrayMap}

/** For representing features over bits */
object BitStuff {
  sealed class LabelOfBit(val index: Int)
  case object Parent extends LabelOfBit(0)
  case object LChild extends LabelOfBit(1)
  case object RChild extends LabelOfBit(2)
  case object UChild extends LabelOfBit(3)
  val bitLabels = Seq(Parent,LChild,RChild,UChild)
}
import BitStuff._


@SerialVersionUID(1)
trait Feature[+L,+W] extends Serializable

/**
 * A Featurizer turns decisions in a grammar into a set of features, possibly weighted
 * @author dlwh
 */
@SerialVersionUID(1)
trait Featurizer[L,W] extends Serializable {
  def featuresFor(r: Rule[L]):Counter[Feature[L,W], Double]
  def featuresFor(l: L, w: W):Counter[Feature[L,W], Double]

  /** should return 0.0 if we don't care about this feature. */
  def initialValueForFeature(f: Feature[L,W]):Double
}

/** A Rule feature is just an indicator on there being this rule */
case class RuleFeature[L](r: Rule[L]) extends Feature[L,Nothing] with CachedHashCode
/** A simple indicator feature */
case class WeightedFeature(kind: Symbol) extends Feature[Nothing,Nothing] with CachedHashCode
/** A Lexical feature is just an indicator on there being this word */
case class LexicalFeature[L,W](l: L, w: W) extends Feature[L,W] with CachedHashCode
/** wraps a feature with substate information */
case class SubstateFeature[L,W,T](f: Feature[L,W], states: Seq[T]) extends Feature[(L,T),W] with CachedHashCode
case class UnannotatedRuleFeature[L](r: Rule[L]) extends Feature[L,Nothing] with CachedHashCode

/** Useful for the all-bits featurizer: does it have this bit on this rule? */
case class BitFeature(lbl: LabelOfBit, index: Int, toggled: Int) extends Feature[Nothing,Nothing] with CachedHashCode
/** conjoins some features */
case class SequenceFeature[L,W](f: Seq[Feature[L,W]]) extends Feature[L,W] with CachedHashCode
/** conjoins just two features*/
case class PairFeature[L,W](f: Feature[L,W], f2: Feature[L,W]) extends Feature[(L,Int),W] with CachedHashCode
case class TaggedFeature[L,W](f: Feature[L,W], symbol: Symbol) extends Feature[L,W] with CachedHashCode


/**
 * Just returns features on the input rules
 */
class SimpleFeaturizer[L,W] extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = Counter(RuleFeature(r) -> 1.0)
  def featuresFor(l: L, w: W) = Counter(LexicalFeature(l,w) -> 1.0)

  def initialValueForFeature(f: Feature[L,W]) = -1.0
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

  def initialValueForFeature(f: Feature[L,W]) = f1.initialValueForFeature(f) + f2.initialValueForFeature(f)
}

/**
 * Strips annotations from a symbol before doing normal featurization
 */
class UnannotatingFeaturizer[W] extends Featurizer[String,W] {
  def featuresFor(r: Rule[String]) = r match {
    case BinaryRule(a,b,c) =>
      Counter(UnannotatedRuleFeature(BinaryRule(unannotate(a),unannotate(b),unannotate(c))) -> 1.0)
    case UnaryRule(a,b) =>
      Counter(UnannotatedRuleFeature(UnaryRule(unannotate(a),unannotate(b))) -> 1.0)
  }

  def featuresFor(l: String, w: W) = Counter[Feature[String,W],Double]()

  def initialValueForFeature(f: Feature[String, W]) = 0.0

  def unannotate(a: String) = {
    a.takeWhile(_ != '^')
  }
}

/**
 * Just returns indicators on rule features, no lexical features
 */
class RuleFeaturizer[L,W](binaries: Counter2[L,BinaryRule[L], Double],
                          unaries: Counter2[L,UnaryRule[L],Double],
                          initToZero: Boolean = true, scale: Double = 1.0) extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = Counter(RuleFeature(r) -> 1.0)
  def featuresFor(l: L, w: W) = Counter[Feature[L,W],Double]()
  val unaryTotals = sum(unaries,Axis.Vertical)
  val binaryTotals = sum(binaries,Axis.Vertical)

  def initialValueForFeature(f: Feature[L,W]) = f match {
    case RuleFeature(r:BinaryRule[L]) => if(initToZero) 0.0  else{
      val s = math.log(binaries(r.parent,r) / binaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite) 0.0 else s
    }
    case RuleFeature(r:UnaryRule[L]) => if(initToZero) 0.0 else {
      val s = math.log(unaries(r.parent,r) / unaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite)  0.0 else s
    }
    case _ => 0.0
  }

}


/**
 * Adds in a feature LogProb with weight equal to the generative log prob of a rule/symbol
 */
class WeightedRuleFeaturizer[L,W](binaries: Counter2[L,BinaryRule[L],Double],
                                  unaries: Counter2[L,UnaryRule[L],Double],
                                  lexicon: Counter2[L,W,Double],
                                  initToZero: Boolean = true, scale: Double = 1.0) extends Featurizer[L,W] {
  val unaryTotals = sum(unaries,Axis.Vertical)
  val binaryTotals = sum(binaries,Axis.Vertical)
  def featuresFor(r: Rule[L]) = r match {
    case u: UnaryRule[L] => Counter(WeightedFeature('LogProb)->math.log(unaries(u.parent,u)/unaryTotals(u.parent)))
    case u: BinaryRule[L] => Counter(WeightedFeature('LogProb)->math.log(binaries(u.parent,u)/binaryTotals(u.parent)))
  }

  val smoothedLexicon: SimpleLexicon[L, W] = new SimpleLexicon(lexicon)

  def featuresFor(l: L, w: W) = {
    Counter(WeightedFeature('LogProb)->smoothedLexicon.wordScore(l,w))
  }


  def initialValueForFeature(f: Feature[L,W]) = f match {
    case RuleFeature(r:BinaryRule[L]) => if(initToZero) 0.0  else{
      val s = math.log(binaries(r.parent,r) / binaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite) 0.0 else s
    }
    case RuleFeature(r:UnaryRule[L]) => if(initToZero) 0.0 else {
      val s = math.log(unaries(r.parent,r) / unaryTotals(r.parent)) / scale //+ math.log(r.hashCode.abs * 1.0 /  Int.MaxValue)
      if(s.isNaN || s.isInfinite)  0.0 else s
    }
    case _ => 0.0
  }


}

/**
 * Forwards feature calls, but looks up known weights for initial values
 */
class CachedWeightsFeaturizer[L,W](f: Featurizer[L,W],
                                   weights: Counter[Feature[L,W], Double],
                                   proj: Feature[L,W]=>Feature[L,W] = identity[Feature[L,W]] _,
                                   randomize:Boolean = true, randomizeZeros: Boolean = false) extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) =  f.featuresFor(r)

  def featuresFor(l: L, w: W) = f.featuresFor(l,w)

  def initialValueForFeature(feat: Feature[L,W]) = {
    weights.get(feat) orElse weights.get(proj(feat)) match {
      case Some(v) =>
        v + {if(randomize || (v == 0.0 && randomizeZeros)) math.log(0.99 + math.random * 0.02) else 0.0}
      case None =>  f.initialValueForFeature(feat)
    }
  }
}

object FeatureProjectors {
  def split[L,W](f: Feature[(L,Int),W], splitFactor: Int) = f match {
    case SubstateFeature(k,states) => SubstateFeature(k,states.map(_/splitFactor))
    case _ => f
  }
}


/**
 * Takes another featurizer and wraps it in a SubstateFeature(baseFeature,<vector of parameters>)
 */
class SlavFeaturizer[L,W](base: Featurizer[L,W], numStates:Int) extends Featurizer[(L,Int),W] {
  def featuresFor(r: Rule[(L,Int)]) = r match {
    case BinaryRule(a,b,c) =>
      val result = Counter[Feature[(L,Int),W],Double]()
      val baseFeatures = base.featuresFor(BinaryRule(a._1,b._1,c._1))
      val substates = ArrayBuffer(a._2, b._2, c._2)
      for( (k,v) <- baseFeatures.nonzero.pairs) {
        result(SubstateFeature(k,substates)) = v
      }
      result
    case UnaryRule(a,b) =>
      val result = Counter[Feature[(L,Int),W],Double]()
      val baseFeatures = base.featuresFor(UnaryRule(a._1,b._1))
      val substates = ArrayBuffer(a._2,b._2)
      for( (k,v) <- baseFeatures.nonzero.pairs) {
        result(SubstateFeature(k,substates)) = v
      }
      result
  }

  def featuresFor(l: (L,Int), w: W) = {
    val baseFeatures = base.featuresFor(l._1, w)
    val substates = ArrayBuffer(l._2)
    val result = Counter[Feature[(L,Int),W],Double]()
    for( (k,v) <- baseFeatures.nonzero.pairs) {
      result(SubstateFeature(k,substates)) = v
    }
    result
  }

  def initialValueForFeature(f: Feature[(L,Int),W]) = f match {
    case SubstateFeature(baseF, x) =>
      val baseScore = base.initialValueForFeature(baseF) //+ math.log(x.foldLeft(1.)(_ + 3 * _))
      assert(!baseScore.isNaN,baseF)
      //baseScore + math.log(1.0 - 1E-10 + math.random * 2 * 1E-10)
      val r = baseScore + math.log(1.0 - .1 + math.random * 2 * .1)
      assert(!r.isNaN,"post random: " + baseF)
      r
    case _ => 0.0
  }
}

case class IndexFeature[L2,W](f: Feature[L2,W], index:Int) extends Feature[(Nothing,Seq[L2]),W]

/**
 * Featurizer for training products of L2 grammars that project to L's.
 * Elements of the product are independent, given the L.
 */
class ProductFeaturizer[L,L2,W](base: IndexedSeq[Featurizer[L2,W]]) extends Featurizer[(L,Seq[L2]),W] {
  def featuresFor(r: Rule[(L, Seq[L2])]) = r match {
    case BinaryRule(a,b,c) =>
      val ctr = Counter[Feature[(L,Seq[L2]),W],Double]()
      for( i <- 0 until base.length) {
        val baseFeatures = base(i).featuresFor(BinaryRule(a._2 apply (i),b._2 apply (i),c._2 apply (i)))
        for( (f,v) <- baseFeatures.pairsIterator) {
          ctr(IndexFeature(f,i)) = v
        }

      }
      ctr
    case UnaryRule(a,b) =>
      val ctr = Counter[Feature[(L,Seq[L2]),W],Double]()
      for( i <- 0 until base.length) {
        val baseFeatures = base(i).featuresFor(UnaryRule(a._2 apply (i),b._2 apply (i)))
        for( (f,v) <- baseFeatures.pairsIterator) {
          ctr(IndexFeature(f,i)) = v
        }

      }
      ctr
  }

  def featuresFor(l: (L, Seq[L2]), w: W) = {
    val ctr = Counter[Feature[(L,Seq[L2]),W],Double]()
      for( i <- 0 until base.length) {
        val baseFeatures = base(i).featuresFor(l._2 apply i,w)
        for( (f,v) <- baseFeatures.pairsIterator) {
          ctr(IndexFeature(f,i)) = v
        }

      }
      ctr
  }

  def initialValueForFeature(f: Feature[(L, Seq[L2]), W]) = f match  {
    case IndexFeature(feat,i) => base(i).initialValueForFeature(feat)
  }
}

/**
 * Slav Featurizer that includes features for the base state
 */
class SlavPlusFeaturizer[L,W](base: Featurizer[L,W], numStates:Int) extends Featurizer[(L,Int),W] {
  case class ProjFeature(f: Feature[L,W]) extends Feature[(L,Int),W]

  def featuresFor(r: Rule[(L,Int)]) = r match {
    case BinaryRule(a,b,c) =>
      val result = Counter[Feature[(L,Int),W],Double]()
      val baseFeatures = base.featuresFor(BinaryRule(a._1,b._1,c._1))
      val substates = ArrayBuffer(a._2, b._2, c._2)
      for( (k,v) <- baseFeatures.nonzero.pairs) {
        result(SubstateFeature(k,substates)) = v
        result(ProjFeature(k)) = v
      }
      result
    case UnaryRule(a,b) =>
      val result = Counter[Feature[(L,Int),W],Double]()
      val baseFeatures = base.featuresFor(UnaryRule(a._1,b._1))
      val substates = ArrayBuffer(a._2,b._2)
      for( (k,v) <- baseFeatures.nonzero.pairs) {
        result(SubstateFeature(k,substates)) = v
        result(ProjFeature(k)) = v
      }
      result
  }

  def featuresFor(l: (L,Int), w: W) = {
    val baseFeatures = base.featuresFor(l._1, w)
    val substates = ArrayBuffer(l._2)
    val result = Counter[Feature[(L,Int),W],Double]()
    for( (k,v) <- baseFeatures.nonzero.pairs) {
      result(SubstateFeature(k,substates)) = v
      result(ProjFeature(k)) = v
    }
    result
  }
  def initialValueForFeature(f: Feature[(L,Int),W]) = f match {
    case SubstateFeature(baseF, _) =>
      val baseScore = base.initialValueForFeature(baseF)
//      baseScore + math.log(0.99 + math.random * 0.02)
      baseScore + math.log(1.0 - 1E-10 + math.random * 2 * 1E-10)
    case _ => 0.0
  }
}

/**
 * Treats the Substate as a bit vector and produces up to arity subset features
 * based on that bitvector
 */
class BitVectorFeaturizer[L,W](base: Featurizer[L,W], numStates: Int, arity: Int = 1) extends Featurizer[(L,Int),W] {
  val numBits = BitUtils.roundToNextPowerOfTwo(numStates)
  private def mkBitStati(numBits: Int, lbl: LabelOfBit, state: Int) = {
    for( (bit,toggled) <- BitUtils.iterateBits(state,numBits)) yield {
      stati(lbl.index)(bit)(toggled)
    }
  }

  private val stati = Array.tabulate(4,numBits,2) { (parentId, bit, toggled) =>
    BitFeature(bitLabels(parentId),bit,toggled)
  }

  // all subsets <= size k
  def chooseSubsets[X,Y](fs: Seq[Feature[X,Y]], myArity:Int = arity):Seq[Seq[Feature[X,Y]]] = {
    if(fs.isEmpty || myArity == 0) Seq(Seq.empty)
    else {
      val rec = chooseSubsets(fs.tail,myArity-1)
      rec ++ rec.map(fs.head +: _)
    }
  }


  def featuresFor(r: Rule[(L,Int)]) = r match {
    case BinaryRule(a,b,c) =>
      val result = Counter[Feature[(L,Int),W],Double]()
      val baseFeatures = base.featuresFor(BinaryRule(a._1,b._1,c._1))
      val subFeatures = {
        val bitIterator = mkBitStati(numBits,Parent,a._2) ++ mkBitStati(numBits,LChild,b._2) ++ mkBitStati(numBits, RChild, c._2)
        val subsets = chooseSubsets(bitIterator.toSeq)
        subsets.map(SequenceFeature(_))
      }
      for( (k,v) <- baseFeatures.nonzero.pairs; sub <- subFeatures) {
        result(PairFeature(k,sub)) = v
      }
      result
    case UnaryRule(a,b) =>
      val result = Counter[Feature[(L,Int),W],Double]()
      val baseFeatures = base.featuresFor(UnaryRule(a._1,b._1))
      val subFeatures = {
        val bitIterator = mkBitStati(numBits,Parent,a._2) ++ mkBitStati(numBits,UChild,b._2)
        val subsets = chooseSubsets(bitIterator.toSeq)
        subsets.map(SequenceFeature(_))
      }
      for( (k,v) <- baseFeatures.nonzero.pairs; sub <- subFeatures) {
        result(PairFeature(k,sub)) = v
      }
      result
  }

  def featuresFor(l: (L,Int), w: W) = {
    val baseFeatures = base.featuresFor(l._1, w)
    val result = Counter[Feature[(L,Int),W],Double]()
    val subFeatures = {
      val bitIterator = mkBitStati(numBits,Parent,l._2)
      val subsets = chooseSubsets(bitIterator.toSeq)
      subsets.map(SequenceFeature(_))
    }
    for( (k,v) <- baseFeatures.nonzero.pairs; sub <- subFeatures) {
      result(PairFeature(k,sub)) = v
    }
    result
  }

  def initialValueForFeature(f: Feature[(L,Int),W]) = f match {
    case PairFeature(baseF, SequenceFeature(Seq())) =>
      val baseScore = base.initialValueForFeature(baseF)
      baseScore
    case _ : PairFeature[_,_] =>
      math.log(0.9 + math.random * 0.2)
    case _ => 0.0
  }

}


/**
 * FeatureIndexers give you an indexed encoding of the features for each rule and label
 * using indexed rule and indexed labels.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait FeatureIndexer[L,W] extends Encoder[Feature[L,W]] with Serializable {
  val index:Index[Feature[L,W]]
  val labelIndex: Index[L]
  val ruleIndex: Index[Rule[L]]
  val featurizer: Featurizer[L,W]

  // r -> SparseVector[Double] of feature weights
  val ruleCache: Array[SparseVector[Double]]
  // a -> W map
  val lexicalCache: Array[Map[W,SparseVector[Double]]]


  def featuresFor(r: Int) = {
    ruleCache(r)
  }

  def featuresFor(a: Int, w: W) = {
    if(!lexicalCache(a).contains(w)) {
      stripEncode(featurizer.featuresFor(labelIndex.get(a),w))
    }
    else lexicalCache(a)(w)
  }

  def initialValueFor(f: Feature[L,W]):Double = featurizer.initialValueForFeature(f)

  def initialValueFor(f: Int):Double = initialValueFor(index.get(f))

  // strips out features we haven't seen before.
  private def stripEncode(ctr: Counter[Feature[L,W], Double]) = {
    val res = mkSparseVector()
    for( (k,v) <- ctr.nonzero.pairs) {
      val ind = index(k)
      if(ind != -1) {
        res(ind) = v
      }
    }
    res
  }
}

object FeatureIndexer {

  /**
   * Creates a FeatureIndexer by featurizing all rules/words and indexing them
   */
  def apply[L,L2,W](f: Featurizer[L2,W], lex: Lexicon[L,W], indexedProjections: GrammarProjections[L,L2]) = {
    val featureIndex = Index[Feature[L2,W]]()
    val ruleIndex = indexedProjections.rules.fineIndex

    // a -> b c -> SparseVector[Double] of feature weights
    val ruleCache = new OpenAddressHashArray[Counter[Feature[L2,W],Double]](Int.MaxValue/3)
    // a -> W map
    val lexicalCache = new ArrayMap(collection.mutable.Map[W,Counter[Feature[L2,W], Double]]())

    // rules
    for (rule <- indexedProjections.rules.fineIndex) {
      val feats = f.featuresFor(rule)
      val ri = ruleIndex(rule)
      ruleCache(ri) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    // lex
    for {
      (l,w) <- lex.knownTagWords
      lSplit <- indexedProjections.labels.refinementsOf(l)
    } {
      val feats = f.featuresFor(lSplit,w)
      lexicalCache(indexedProjections.labels.fineIndex(lSplit))(w) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    cachedFeaturesToIndexedFeatures[L2,W](f,indexedProjections.labels.fineIndex,ruleIndex,featureIndex,ruleCache,lexicalCache)
  }

  private def cachedFeaturesToIndexedFeatures[L,W](f: Featurizer[L,W], lI: Index[L], rI: Index[Rule[L]], featureIndex: Index[Feature[L,W]],
                                        ruleCache: OpenAddressHashArray[Counter[Feature[L,W],Double]],
                                        lexicalCache: ArrayMap[collection.mutable.Map[W,Counter[Feature[L,W], Double]]]) = {
      val featureEncoder = Encoder.fromIndex(featureIndex)
      val brc =  Array.tabulate(rI.size){ r =>
        featureEncoder.encodeSparse(ruleCache(r))
      }

      val lrc = Array.tabulate(lI.size){ (a) =>
        lexicalCache(a).mapValues(featureEncoder.encodeSparse _).toMap
      }

      new FeatureIndexer[L,W] {
        val index = featureIndex
        val labelIndex = lI
        val ruleIndex = rI
        val featurizer = f

        // a -> b c -> SparseVector[Double] of feature weights
        val ruleCache = brc
        // a -> W map
        val lexicalCache: Array[Map[W,SparseVector[Double]]] = lrc
      }
    }

}

