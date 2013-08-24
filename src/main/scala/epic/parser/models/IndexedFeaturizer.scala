package epic.parser
package models

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import epic.framework._
import breeze.util._
import projections._
import epic.parser.features.{SimpleFeaturizer, Featurizer}
import breeze.collection.mutable.{ArrayMap, OpenAddressHashArray}
import epic.trees.{TreeInstance, LexicalProduction}
import breeze.linalg.DenseVector
import collection.mutable.ArrayBuilder
import epic.lexicon.Lexicon
import epic.constraints.TagConstraints

/**
 * [[epic.parser.models.IndexedFeaturizer]] are featurizers for "normal" unanchored grammars.
 * They define an indexed encoding of the features for each rule and label
 * using indexed rule and indexed labels. Handles [[epic.parser.features.Featurizer]] instances
 *
 * @author dlwh
 */
@SerialVersionUID(1)
trait IndexedFeaturizer[L, L2, W] extends RefinedFeaturizer[L, W, Feature] with Encoder[Feature] with Serializable { outer =>
  val index: Index[Feature]
  val featurizer: SimpleFeaturizer[L2, W]
  val grammar: BaseGrammar[L]
  val proj: GrammarRefinements[L, L2]

  def labelIndex = proj.labels.fineIndex

  def ruleIndex = proj.rules.fineIndex

  // r -> SparseVector[Double] of feature weights
  val ruleCache: Array[Array[Int]]


  def featuresFor(r: Int) = {
    ruleCache(r)
  }

  def computeWeight(r: Int, weights: DenseVector[Double]): Double = dot(featuresFor(r),weights)

  private def dot(features: Array[Int], weights: DenseVector[Double]) = {
    var i = 0
    var score = 0.0
    while(i < features.length) {
      score += weights(features(i))
      i += 1
    }
    score
  }

  def anchor(words: IndexedSeq[W]) = new Spec(words)

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

  case class Spec private[IndexedFeaturizer](words: IndexedSeq[W]) extends super.Anchoring {
    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      val globalRule = proj.rules.globalize(rule, ref)
      outer.featuresFor(globalRule)
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val globalRule = proj.rules.globalize(rule, ref)
      outer.featuresFor(globalRule)
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      if (begin + 1 == end) {
        val globalTag = proj.labels.globalize(tag, ref)
        featuresFor(begin, globalTag)
      } else Array.empty[Int]
    }

    val innerAnch = featurizer.anchor(words)

    def featuresFor(pos: Int, a: Int) = {
      stripEncode(innerAnch.featuresFor(pos, labelIndex.get(a)))
    }


    def computeWeight(pos: Int, l: Int, weights: DenseVector[Double]) = dot(featuresFor(pos, l), weights)
  }

}

object IndexedFeaturizer {

  /**
   * Creates a FeatureIndexer by featurizing all rules/words and indexing them
   */
  def apply[L, L2, W](grammar: BaseGrammar[L],
                      lexicon: TagConstraints.Factory[L, W],
                      trees: IndexedSeq[TreeInstance[L, W]],
                      f: SimpleFeaturizer[L2, W],
                      indexedProjections: GrammarRefinements[L, L2]): IndexedFeaturizer[L, L2, W] = {
    val featureIndex = Index[Feature]()
    val ruleIndex = indexedProjections.rules.fineIndex

    // a -> b c -> SparseVector[Double] of feature weights
    val ruleCache = new OpenAddressHashArray[Array[Feature]](Int.MaxValue/3)

    // rules
    for (rule <- indexedProjections.rules.fineIndex) {
      val feats = f.featuresFor(rule)
      val ri = ruleIndex(rule)
      ruleCache(ri) = feats
      feats.foreach {featureIndex.index }
    }

    // lex
    for {
      ex <- trees
      featAnch = f.anchor(ex.words)
      lexLoc = lexicon.anchor(ex.words)
      i <- 0 until ex.words.length
      l <- lexLoc.allowedTags(i)
      lSplit <- indexedProjections.labels.refinementsOf(l)
    } {
      val feats = featAnch.featuresFor(i, indexedProjections.labels.fineIndex.get(lSplit))
      feats.foreach {featureIndex.index _ }
    }

    cachedFeaturesToIndexedFeatures[L, L2, W](grammar, lexicon, indexedProjections, f, featureIndex, ruleCache)
  }

  private def cachedFeaturesToIndexedFeatures[L, L2, W](grammar: BaseGrammar[L], lexicon: TagConstraints.Factory[L, W],
                                                        refinements: GrammarRefinements[L, L2],
                                                        f: SimpleFeaturizer[L2, W],
                                                        featureIndex: Index[Feature],
                                                        ruleCache: OpenAddressHashArray[Array[Feature]]): IndexedFeaturizer[L, L2, W]  = {
    val brc =  Array.tabulate(refinements.rules.fineIndex.size){ r =>
      ruleCache(r) map featureIndex
    }

    val g = grammar
    val l = lexicon

    new IndexedFeaturizer[L, L2, W] {
      val index = featureIndex
      val featurizer = f

      val grammar = g
      val lexicon = l
      val proj = refinements
      val ruleCache = brc
    }
  }

}
