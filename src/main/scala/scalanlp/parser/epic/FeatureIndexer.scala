package scalanlp.parser
package epic

import scalanlp.util.{Encoder, Index}

import projections._
import scalala.tensor.Counter
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.DenseVector
import scalanlp.collection.mutable.{ArrayMap, OpenAddressHashArray}

/**
 * FeatureIndexers give you an indexed encoding of the features for each rule and label
 * using indexed rule and indexed labels.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait FeatureIndexer[L,W] extends Encoder[Feature] with Serializable {
  val index:Index[Feature]
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

  def dotProjectOfFeatures(a: Int, w: W, weights: DenseVector[Double]) = {
    if(lexicalCache(a).contains(w)) lexicalCache(a)(w) dot weights
    else {
      var score = 0.0
      val feats = featurizer.featuresFor(labelIndex.get(a),w)
      for( (k,v) <- feats.nonzero.pairs) {
        val ind = index(k)
        if(ind != -1) {
          score += v * weights(ind)
        }
      }
      score
    }


  }

  def initialValueFor(f: Feature):Double = featurizer.initialValueForFeature(f)

  def initialValueFor(f: Int):Double = initialValueFor(index.get(f))

  // strips out features we haven't seen before.
  private def stripEncode(ctr: Counter[Feature, Double]) = {
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

  def apply[L2,W](f: Featurizer[L2,W], lex: Iterable[(L2,W)], grammar: Grammar[L2]) = {
    val featureIndex = Index[Feature]()
    val ruleIndex = grammar.index;

    // a -> b c -> SparseVector[Double] of feature weights
    val ruleCache = new OpenAddressHashArray[Counter[Feature,Double]](Int.MaxValue/3)
    // a -> W map
    val lexicalCache = new ArrayMap(collection.mutable.Map[W,Counter[Feature, Double]]())

    // rules
    for (rule <- ruleIndex) {
      val feats = f.featuresFor(rule)
      val ri = ruleIndex(rule)
      ruleCache(ri) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    // lex
    for {
      (l,w) <- lex
    } {
      val feats = f.featuresFor(l,w)
      lexicalCache(grammar.labelIndex(l))(w) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    cachedFeaturesToIndexedFeatures[L2,W](f,grammar.labelIndex,ruleIndex,featureIndex,ruleCache,lexicalCache)
  }

  /**
   * Creates a FeatureIndexer by featurizing all rules/words and indexing them
   */
  def apply[L,L2,W](f: Featurizer[L2,W], lex: Iterable[(L2,W)], indexedProjections: GrammarProjections[L,L2]) = {
    val featureIndex = Index[Feature]()
    val ruleIndex = indexedProjections.rules.fineIndex

    // a -> b c -> SparseVector[Double] of feature weights
    val ruleCache = new OpenAddressHashArray[Counter[Feature,Double]](Int.MaxValue/3)
    // a -> W map
    val lexicalCache = new ArrayMap(collection.mutable.Map[W,Counter[Feature, Double]]())

    // rules
    for (rule <- indexedProjections.rules.fineIndex) {
      val feats = f.featuresFor(rule)
      val ri = ruleIndex(rule)
      ruleCache(ri) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    // lex
    for {
      (lSplit,w) <- lex
    } {
      val feats = f.featuresFor(lSplit,w)
      lexicalCache(indexedProjections.labels.fineIndex(lSplit))(w) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    cachedFeaturesToIndexedFeatures[L2,W](f,indexedProjections.labels.fineIndex,ruleIndex,featureIndex,ruleCache,lexicalCache)
  }

  private def cachedFeaturesToIndexedFeatures[L,W](f: Featurizer[L,W], lI: Index[L], rI: Index[Rule[L]], featureIndex: Index[Feature],
                                        ruleCache: OpenAddressHashArray[Counter[Feature,Double]],
                                        lexicalCache: ArrayMap[collection.mutable.Map[W,Counter[Feature, Double]]]) = {
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

