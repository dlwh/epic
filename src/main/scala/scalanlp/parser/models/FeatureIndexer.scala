package scalanlp.parser
package models

import scalanlp.epic._
import scalanlp.util._
import projections._
import features.Featurizer
import scalanlp.collection.mutable.{ArrayMap, OpenAddressHashArray}
import scalanlp.trees.LexicalProduction
import scalala.tensor.dense.DenseVector
import collection.mutable.ArrayBuilder

/**
 * FeatureIndexers give you an indexed encoding of the features for each rule and label
 * using indexed rule and indexed labels. Handles Featurizers
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
  val ruleCache: Array[Array[Int]]
  // a -> W map
  val lexicalCache: Array[Map[W, Array[Int]]]


  def featuresFor(r: Int) = {
    ruleCache(r)
  }

  def featuresFor(a: Int, w: W) = {
    if (!lexicalCache(a).contains(w)) {
      stripEncode(featurizer.featuresFor(labelIndex.get(a), w))
    }
    else lexicalCache(a)(w)
  }

  def computeWeight(r: Int, weights: DenseVector[Double]): Double = dot(featuresFor(r),weights)
  def computeWeight(l: Int, w: W, weights: DenseVector[Double]) = dot(featuresFor(l, w), weights)

  private def dot(features: Array[Int], weights: DenseVector[Double]) = {
    var i = 0
    var score = 0.0
    while(i < features.length) {
      score += weights(features(i))
      i += 1
    }
    score
  }

  def dotProjectOfFeatures(a: Int, w: W, weights: DenseVector[Double]) = {
    if (lexicalCache(a).contains(w)) dot(lexicalCache(a)(w), weights)
    else {
      var score = 0.0
      val feats = featurizer.featuresFor(labelIndex.get(a), w)
      for ( k <- feats) {
        val ind = index(k)
        if (ind != -1) {
          score += weights(ind)
        }
      }
      score
    }
  }


  def specialize(words: Seq[W]) = new Spec(words)

  def initialValueFor(f: Feature): Double = featurizer.initialValueForFeature(f)

  def initialValueFor(f: Int): Double = initialValueFor(index.get(f))

  // strips out features we haven't seen before.
  private def stripEncode(ctr: Array[Feature]) = {
    val result = ArrayBuilder.make[Int]
    result.sizeHint(ctr.length)
    for (k <- ctr) {
      val ind = index(k)
      if (ind != -1) {
        result += ind
      }
    }
    result.result
  }

  case class Spec private[FeatureIndexer](words: Seq[W]) extends super.Specialization {
    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      val globalRule = proj.rules.globalize(rule, ref)
      featuresFor(globalRule)
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val globalRule = proj.rules.globalize(rule, ref)
      featuresFor(globalRule)
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      if (begin + 1 == end) {
        val globalTag = proj.labels.globalize(tag, ref)
        featuresFor(globalTag, words(begin))
      } else Array.empty[Int]
    }
  }

}

object FeatureIndexer {

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
    val ruleCache = new OpenAddressHashArray[Array[Feature]](Int.MaxValue/3)
    // a -> W map
    val lexicalCache = new ArrayMap(collection.mutable.Map[W, Array[Feature]]())

    // rules
    for (rule <- indexedProjections.rules.fineIndex) {
      val feats = f.featuresFor(rule)
      val ri = ruleIndex(rule)
      ruleCache(ri) = feats
      feats.foreach {featureIndex.index _ }
    }

    // lex
    for {
      LexicalProduction(l, w) <- lexicon.knownLexicalProductions
      lSplit <- indexedProjections.labels.refinementsOf(l)
    } {
      val feats = f.featuresFor(lSplit, w)
      lexicalCache(indexedProjections.labels.fineIndex(lSplit))(w) = feats
      feats.foreach {featureIndex.index _ }
    }

    cachedFeaturesToIndexedFeatures[L, L2, W](grammar, indexedProjections, f, featureIndex, ruleCache, lexicalCache.map{ case (k,v) => k -> (Map.empty ++ v)}.toMap)
  }

  private def cachedFeaturesToIndexedFeatures[L, L2, W](grammar: Grammar[L],
                                                        refinements: GrammarRefinements[L, L2],
                                                        f: Featurizer[L2, W],
                                                        featureIndex: Index[Feature],
                                                        ruleCache: OpenAddressHashArray[Array[Feature]],
                                                        lexicalCache: Map[Int, Map[W, Array[Feature]]]) = {
    val brc =  Array.tabulate(refinements.rules.fineIndex.size){ r =>
      ruleCache(r) map featureIndex
    }

    val lrc = Array.tabulate(refinements.labels.fineIndex.size){ (a) =>
      lexicalCache(a).map{ case (k,v) => k -> (v map featureIndex)}
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
      val lexicalCache = lrc
    }
  }

}
