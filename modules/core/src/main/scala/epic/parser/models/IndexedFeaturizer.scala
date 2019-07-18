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
import epic.trees.{BinarizedTree, TreeInstance}
import breeze.linalg.DenseVector
import epic.features.{HashFeature, CrossProductIndex, IndexedWordFeaturizer}
import breeze.features.FeatureVector

/**
 * [[epic.parser.models.IndexedFeaturizer]] are featurizers for "normal" unanchored grammars.
 * They define an indexed encoding of the features for each rule and label
 * using indexed rule and indexed labels. Handles [[epic.parser.ProductionFeaturizer]] instances
 *
 * @author dlwh
 */
@SerialVersionUID(2)
class IndexedFeaturizer[L, L2, W](val index: CrossProductIndex[Feature, Feature],
                                   feat: ProductionFeaturizer[L, L2, W],
                                   wGen: IndexedWordFeaturizer[W],
                                   indexedProjections: GrammarRefinements[L, L2],
                                   ruleCache: Array[Array[Int]]) extends RefinedFeaturizer[L, W, Feature] with Encoder[Feature] with Serializable { outer =>

  import indexedProjections._

  def labelIndex = labels.fineIndex

  override def lock = new IndexedFeaturizer(index.lock, feat, wGen, indexedProjections, ruleCache)

  def computeWeight(r: Int, weights: DenseVector[Double]): Double = new FeatureVector(ruleCache(r)) dot weights

  def anchor(words: IndexedSeq[W]) = new Spec(words)

  case class Spec private[IndexedFeaturizer](words: IndexedSeq[W]) extends super.Anchoring {
    val anch = wGen.anchor(words)

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      ruleCache(rules.globalize(rule, ref))
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      ruleCache(rules.globalize(rule, ref))
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      if (begin + 1 == end) {
        val globalTag = labels.globalize(tag, ref)
        featuresFor(begin, globalTag)
      } else {
        Array.empty[Int]
      }
    }

    def featuresFor(pos: Int, tag: Int):Array[Int] = {
      index.crossProduct(feat.featuresForLabel(tag), anch.featuresForWord(pos), usePlainLabelFeatures = false)
    }

    def computeWeight(pos: Int, l: Int, weights: DenseVector[Double]) = new FeatureVector(featuresFor(pos, l)) dot weights
  }

}

object IndexedFeaturizer {

  /**
   * Creates a FeatureIndexer by featurizing all rules/words and indexing them
   */
  def apply[L, L2, W](feat: ProductionFeaturizer[L, L2, W],
                      wGen: IndexedWordFeaturizer[W],
                      trees: IndexedSeq[TreeInstance[L, W]],
                      annotate: (TreeInstance[L, W])=>BinarizedTree[IndexedSeq[L2]],
                      indexedProjections: GrammarRefinements[L, L2]): IndexedFeaturizer[L, L2, W] = {
    val ruleIndex = indexedProjections.rules.fineIndex

    val ruleCache = new Array[Array[Int]](indexedProjections.rules.fineIndex.size)

    val lexIndexBuilder = new CrossProductIndex.Builder(feat.index, wGen.featureIndex, hashFeatures = HashFeature.Relative(1.0), includeLabelOnlyFeatures = true)

    // rules
    for (rule <- indexedProjections.rules.fineIndex) {
      val feats = feat.featuresFor(rule)
      val ri = ruleIndex(rule)
      ruleCache(ri) = feats
    }


    // lex
    for {
      ex <- trees
      wanch = wGen.anchor(ex.words)
      preterminals <- annotate(ex).leaves
      wfeats = wanch.featuresForWord(preterminals.span.begin)
      l <- preterminals.label
    } {
      lexIndexBuilder.add(feat.featuresForLabel(indexedProjections.labels.fineIndex(l)), wfeats)
    }

    new IndexedFeaturizer(lexIndexBuilder.result(), feat, wGen, indexedProjections, ruleCache)
  }

}
