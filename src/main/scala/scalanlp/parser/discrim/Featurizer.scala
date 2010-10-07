package scalanlp.parser.discrim

import scalala.tensor.counters.Counters._
import scalanlp.collection.mutable.{SparseArrayMap, SparseArray, ArrayMap}

import scalala.tensor.sparse.SparseVector;
import scalanlp.parser._;
import scalanlp.trees._;
import scalanlp.util.Encoder;
import scalanlp.util.Index;


trait Feature[+L,+W];

/**
 * 
 * @author dlwh
 */
trait Featurizer[L,W] {
  def featuresFor(r: Rule[L]):DoubleCounter[Feature[L,W]];
  def featuresFor(l: L, w: W):DoubleCounter[Feature[L,W]];

  /** should return 0.0 if we don't care about this feature. */
  def initialValueForFeature(f: Feature[L,W]):Double;
}

/** A Rule feature is just an indicator on there being this rule */
case class RuleFeature[L](r: Rule[L]) extends Feature[L,Nothing];
/** A Lexical feature is just an indicator on there being this word */
case class LexicalFeature[L,W](l: L, w: W) extends Feature[L,W];

class SimpleFeaturizer[L,W] extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = aggregate(RuleFeature(r) -> 1.0);
  def featuresFor(l: L, w: W) = aggregate(LexicalFeature(l,w) -> 1.0);

  def initialValueForFeature(f: Feature[L,W]) = -1.0;
}

import scalala.Scalala._;
/** Returns the sum of all features for two featurizers.  */
class SumFeaturizer[L,W](f1: Featurizer[L,W], f2: Featurizer[L,W]) extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = f1.featuresFor(r) + f2.featuresFor(r) value;
  def featuresFor(l: L, w: W)  = f1.featuresFor(l,w) + f2.featuresFor(l,w) value;

  def initialValueForFeature(f: Feature[L,W]) = f1.initialValueForFeature(f) + f2.initialValueForFeature(f);
}

class RuleFeaturizer[L,W](prods: PairedDoubleCounter[L,Rule[L]]) extends Featurizer[L,W] {
  def featuresFor(r: Rule[L]) = aggregate(RuleFeature(r) -> 1.0);
  def featuresFor(l: L, w: W) = aggregate[Feature[L,W]]();


  def initialValueForFeature(f: Feature[L,W]) = f match {
    case RuleFeature(r) => math.log(prods(r.parent,r) / prods(r.parent).total);
    case _ => 0.0;
  }

}

trait FeatureIndexer[L,W] extends Encoder[Feature[L,W]] {
  val index:Index[Feature[L,W]];
  val labelIndex: Index[L];
  val featurizer: Featurizer[L,W];

  // a -> b c -> SparseVector of feature weights
  val binaryRuleCache: Array[SparseArray[SparseArray[SparseVector]]]
  // a -> b SparseVector
  val unaryRuleCache: Array[SparseArray[SparseVector]]
  // a -> W map
  val lexicalCache: Array[Map[W,SparseVector]]


  def featuresFor(a: Int, b: Int, c: Int) = {
    if(binaryRuleCache(a)(b)(c) == null)
      stripEncode(featurizer.featuresFor(BinaryRule(labelIndex.get(a),labelIndex.get(b), labelIndex.get(c))));
    else binaryRuleCache(a)(b)(c);
  }

  def featuresFor(a: Int, b: Int) = {
    if(unaryRuleCache(a)(b) == null) stripEncode(featurizer.featuresFor(UnaryRule(labelIndex.get(a),labelIndex.get(b))));
    else unaryRuleCache(a)(b);
  }

  def featuresFor(a: Int, w: W) = {
    if(!lexicalCache(a).contains(w)) stripEncode(featurizer.featuresFor(labelIndex.get(a),w));
    else lexicalCache(a)(w);
  }

  def initialValueFor(f: Feature[L,W]):Double = featurizer.initialValueForFeature(f);

  def initialValueFor(f: Int):Double = initialValueFor(index.get(f));

  // strips out features we haven't seen before.
  private def stripEncode(ctr: DoubleCounter[Feature[L,W]]) = {
    val res = mkSparseVector();
    for( (k,v) <- ctr) {
      val ind = index(k);
      if(ind != -1) {
        res(ind) = v;
      }
    }
    res;
  }
}

object FeatureIndexer {

  def apply[L,L2,W](f: Featurizer[L2,W], rawGrammar: Grammar[L], lex: Lexicon[L,W], split: L=>Seq[L2]) = {
    val featureIndex = Index[Feature[L2,W]]();
    val splitLabelIndex = Index[L2]();

    // a -> b c -> SparseVector of feature weights
    val binaryRuleCache = new ArrayMap(new SparseArrayMap(new SparseArrayMap[DoubleCounter[Feature[L2,W]]](null)));
    // a -> b SparseVector
    val unaryRuleCache = new ArrayMap(new SparseArrayMap[DoubleCounter[Feature[L2,W]]](null));
    // a -> W map
    val lexicalCache = new ArrayMap(collection.mutable.Map[W,DoubleCounter[Feature[L2,W]]]());

    // binaries
    for{
      (b,binaryRules) <- rawGrammar.allBinaryRules;
      (c,parents) <- binaryRules;
      a <- parents.activeKeys;
      bSplit <- split(rawGrammar.index.get(b));
      val bI = splitLabelIndex.index(bSplit)
      cSplit <- split(rawGrammar.index.get(c));
      val cI = splitLabelIndex.index(cSplit)
      aSplit <- split(rawGrammar.index.get(a))
    } {
      val aI = splitLabelIndex.index(aSplit)
      val binaryRule = BinaryRule(aSplit,bSplit,cSplit);
      val feats = f.featuresFor(binaryRule);
      binaryRuleCache(aI)(bI)(cI) = feats;
      feats.keysIterator.foreach {featureIndex.index _ };
    }


    // unaries
    for{
      (b,parents) <- rawGrammar.allUnaryRules;
      a <- parents.activeKeys;
      bSplit <- split(rawGrammar.index.get(b));
      val bI = splitLabelIndex.index(bSplit)
      aSplit <- split(rawGrammar.index.get(a))
    } {
      val aI = splitLabelIndex.index(aSplit)
      val binaryRule = UnaryRule(aSplit,bSplit);
      val feats = f.featuresFor(binaryRule);
      unaryRuleCache(aI)(bI) = feats;
      feats.keysIterator.foreach {featureIndex.index _ };
    }

    // lex
    for{
      (l,w) <- lex.knownTagWords
      lSplit <- split(l)
    } {
      val lI = splitLabelIndex.index(lSplit)
      val feats = f.featuresFor(lSplit,w);
      lexicalCache(lI)(w) = feats;
      feats.keysIterator.foreach {featureIndex.index _ };
    }

    cachedFeaturesToIndexedFeatures[L2,W](f,splitLabelIndex,featureIndex,binaryRuleCache,unaryRuleCache,lexicalCache)
  }

  def apply[L,W](f: Featurizer[L,W], trees: Iterable[(BinarizedTree[L],Seq[W])]) = {
    val labelIndex = Index[L]();
    val featureIndex = Index[Feature[L,W]]();

    // a -> b c -> SparseVector of feature weights
    val binaryRuleCache = new ArrayMap(new SparseArrayMap(new SparseArrayMap[DoubleCounter[Feature[L,W]]](null)));
    // a -> b SparseVector
    val unaryRuleCache = new ArrayMap(new SparseArrayMap[DoubleCounter[Feature[L,W]]](null));
    // a -> W map
    val lexicalCache = new ArrayMap(collection.mutable.Map[W,DoubleCounter[Feature[L,W]]]());

    for {
      (t,words) <- trees;
      t2 <- t.allChildren
    } {
      t2 match {
        case BinaryTree(a,Tree(b,_),Tree(c,_)) =>
          val ia = labelIndex.index(a);
          val ib = labelIndex.index(b);
          val ic = labelIndex.index(c);
          if(binaryRuleCache(ia)(ib)(ic) eq null) {
            val feats = f.featuresFor(new BinaryRule(a,b,c));
            binaryRuleCache(ia)(ib)(ic) = feats;
            feats.keysIterator.foreach {featureIndex.index _ };
          }
        case UnaryTree(a,Tree(b,_)) =>
          val ia = labelIndex.index(a);
          val ib = labelIndex.index(b);
          if(unaryRuleCache(ia)(ib) eq null) {
            val feats = f.featuresFor(new UnaryRule(a,b));
            unaryRuleCache(ia)(ib) = feats;
            feats.keysIterator.foreach {featureIndex.index _ };
          }
        case n@NullaryTree(a) =>
          val w = words(n.span.start);
          val ia = labelIndex.index(a);
          if(!lexicalCache(ia).contains(w)) {
            val feats = f.featuresFor(a,w);
            lexicalCache(ia)(w) = feats;
            feats.keysIterator.foreach {featureIndex.index _ };
          }

      }

    }

    cachedFeaturesToIndexedFeatures[L,W](f, labelIndex,featureIndex,binaryRuleCache,unaryRuleCache,lexicalCache)
  }

  private def cachedFeaturesToIndexedFeatures[L,W](f: Featurizer[L,W], lI: Index[L], featureIndex: Index[Feature[L,W]],
                                        binaryRuleCache: ArrayMap[SparseArrayMap[SparseArrayMap[DoubleCounter[Feature[L,W]]]]],
                                        unaryRuleCache: ArrayMap[SparseArrayMap[DoubleCounter[Feature[L,W]]]],
                                        lexicalCache: ArrayMap[collection.mutable.Map[W,DoubleCounter[Feature[L,W]]]]) = {
      val featureEncoder = Encoder.fromIndex(featureIndex);
      val brc =  Array.tabulate(lI.size){ a =>
        val bArray = new SparseArray(lI.size, new SparseArray[SparseVector](lI.size, null));
        for((b,cArrayMap) <- binaryRuleCache(a)) {
          for( (c,ctr) <- cArrayMap) {
            bArray(b)(c) = featureEncoder.encodeSparse(ctr);
          }
        }
        bArray;
      };

      val urc = Array.tabulate(lI.size){ a =>
        val bArray =  new SparseArray[SparseVector](lI.size, null);
        for( (b,ctr) <- unaryRuleCache(a))
          bArray(b) = featureEncoder.encodeSparse(ctr);
        bArray;
      }

      val lrc = Array.tabulate(lI.size){ (a) =>
        lexicalCache(a).mapValues(featureEncoder.encodeSparse _).toMap;
      }
      new FeatureIndexer[L,W] {
        val index = featureIndex;
        val labelIndex = lI;
        val featurizer = f;

        // a -> b c -> SparseVector of feature weights
        val binaryRuleCache: Array[SparseArray[SparseArray[SparseVector]]] = brc;
        // a -> b SparseVector
        val unaryRuleCache: Array[SparseArray[SparseVector]] = urc
        // a -> W map
        val lexicalCache: Array[Map[W,SparseVector]] = lrc;

      }
    }

}

