package scalanlp.parser
package models

import scalanlp.epic._
import scalanlp.util._
import projections._
import features.Featurizer
import scalala.tensor.sparse.SparseVector
import scalala.tensor.Counter
import scalanlp.collection.mutable.{ArrayMap, OpenAddressHashArray}
import scalanlp.trees.LexicalProduction
import scalala.tensor.dense.DenseVector

/**
 * FeatureIndexers give you an indexed encoding of the features for each rule and label
 * using indexed rule and indexed labels.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait FeatureIndexer[L, L2, W] extends DerivationFeaturizer[L, W, Feature] with Encoder[Feature] with Serializable {
  val index: Index[Feature]
  val featurizer: Featurizer[L2, W]
  val grammar: Grammar[L]
  val proj: GrammarRefinements[L, L2]

  def labelIndex = proj.labels.fineIndex

  def ruleIndex = proj.rules.fineIndex

  // r -> SparseVector[Double] of feature weights
  val ruleCache: Array[SparseVector[Double]]
  // a -> W map
  val lexicalCache: Array[Map[W, SparseVector[Double]]]


  def featuresFor(r: Int) = {
    ruleCache(r)
  }

  def featuresFor(a: Int, w: W) = {
    if (!lexicalCache(a).contains(w)) {
      stripEncode(featurizer.featuresFor(labelIndex.get(a), w))
    }
    else lexicalCache(a)(w)
  }

  def dotProjectOfFeatures(a: Int, w: W, weights: DenseVector[Double]) = {
    if (lexicalCache(a).contains(w)) lexicalCache(a)(w) dot weights
    else {
      var score = 0.0
      val feats = featurizer.featuresFor(labelIndex.get(a), w)
      for ((k, v) <- feats.nonzero.pairs) {
        val ind = index(k)
        if (ind != -1) {
          score += v * weights(ind)
        }
      }
      score
    }
  }


  def specialize(words: Seq[W]) = new Spec(words)

  def initialValueFor(f: Feature): Double = featurizer.initialValueForFeature(f)

  def initialValueFor(f: Int): Double = initialValueFor(index.get(f))

  // strips out features we haven't seen before.
  private def stripEncode(ctr: Counter[Feature, Double]) = {
    val res = mkSparseVector()
    for ((k, v) <- ctr.nonzero.pairs) {
      val ind = index(k)
      if (ind != -1) {
        res(ind) = v
      }
    }
    res
  }

  case class Spec private[FeatureIndexer](words: Seq[W]) extends super.Specialization {
    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      val globalRule = proj.rules.globalize(rule, ref)
      featuresFor(globalRule).data.indexArray
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val globalRule = proj.rules.globalize(rule, ref)
      featuresFor(globalRule).data.indexArray
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      if (begin + 1 == end) {
        val globalTag = proj.labels.globalize(tag, ref)
        featuresFor(globalTag, words(begin)).data.indexArray
      } else Array.empty[Int]
    }
  }

}

object FeatureIndexer {

  def apply[L, L2, W](grammar: Grammar[L],
                      refinements: GrammarRefinements[L, L2],
                      f: Featurizer[L2, W],
                      lex: Iterable[(L2, W)]) = {
    val featureIndex = Index[Feature]()
    val ruleIndex = refinements.rules.fineIndex
    val labelIndex = refinements.labels.fineIndex

    // a -> b c -> SparseVector[Double] of feature weights
    val ruleCache = new OpenAddressHashArray[Counter[Feature, Double]](Int.MaxValue/3)
    // a -> W map
    val lexicalCache = new ArrayMap(collection.mutable.Map[W, Counter[Feature, Double]]())

    // rules
    for (rule <- ruleIndex) {
      val feats = f.featuresFor(rule)
      val ri = ruleIndex(rule)
      ruleCache(ri) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    // lex
    for {
      (l, w) <- lex
    } {
      val feats = f.featuresFor(l, w)
      lexicalCache(labelIndex(l))(w) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    cachedFeaturesToIndexedFeatures[L, L2, W](grammar, refinements, f, featureIndex, ruleCache, lexicalCache)
  }

  /**
   * Creates a FeatureIndexer by featurizing all rules/words and indexing them
   */
  def apply[L, L2, W](grammar: Grammar[L],
                      lexicon: Lexicon[L, W],
                      f: Featurizer[L2, W],
                      indexedProjections: GrammarRefinements[L, L2]) = {
    val featureIndex = Index[Feature]()
    val ruleIndex = indexedProjections.rules.fineIndex

    // a -> b c -> SparseVector[Double] of feature weights
    val ruleCache = new OpenAddressHashArray[Counter[Feature, Double]](Int.MaxValue/3)
    // a -> W map
    val lexicalCache = new ArrayMap(collection.mutable.Map[W, Counter[Feature, Double]]())

    // rules
    for (rule <- indexedProjections.rules.fineIndex) {
      val feats = f.featuresFor(rule)
      val ri = ruleIndex(rule)
      ruleCache(ri) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    // lex
    for {
      LexicalProduction(l, w) <- lexicon.knownLexicalProductions
      lSplit <- indexedProjections.labels.refinementsOf(l)
    } {
      val feats = f.featuresFor(lSplit, w)
      lexicalCache(indexedProjections.labels.fineIndex(lSplit))(w) = feats
      feats.keysIterator.foreach {featureIndex.index _ }
    }

    cachedFeaturesToIndexedFeatures[L, L2, W](grammar, indexedProjections, f, featureIndex, ruleCache, lexicalCache)
  }

  private def cachedFeaturesToIndexedFeatures[L, L2, W](grammar: Grammar[L],
                                                        refinements: GrammarRefinements[L, L2],
                                                        f: Featurizer[L2, W],
                                                        featureIndex: Index[Feature],
                                                        ruleCache: OpenAddressHashArray[Counter[Feature, Double]],
                                                        lexicalCache: ArrayMap[collection.mutable.Map[W, Counter[Feature, Double]]]) = {
    val featureEncoder = Encoder.fromIndex(featureIndex)
    val brc =  Array.tabulate(refinements.rules.fineIndex.size){ r =>
      featureEncoder.encodeSparse(ruleCache(r))
    }

    val lrc = Array.tabulate(refinements.labels.fineIndex.size){ (a) =>
      lexicalCache(a).mapValues(featureEncoder.encodeSparse _).toMap
    }

    val g = grammar

    new FeatureIndexer[L, L2, W] {
      val index = featureIndex
      val featurizer = f

      // a -> b c -> SparseVector[Double] of feature weights
      val grammar = g
      val proj = refinements
      val ruleCache = brc
      // a -> W map
      val lexicalCache: Array[Map[W, SparseVector[Double]]] = lrc
    }
  }

}
