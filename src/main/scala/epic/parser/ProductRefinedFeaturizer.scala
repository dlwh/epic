package epic.parser
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
import breeze.util.EitherIndex

/**
 *
 * @author dlwh
 */

class ProductRefinedFeaturizer[L, W, Feat1, Feat2](sf1: Grammar[L, W],
                                        sf2: Grammar[L, W],
                                        feat1: RefinedFeaturizer[L, W, Feat1],
                                        feat2: RefinedFeaturizer[L, W, Feat2]) extends RefinedFeaturizer[L, W, Either[Feat1, Feat2]] {
  def index: EitherIndex[Feat1, Feat2] = feat1.index | feat2.index

  override def lock = new ProductRefinedFeaturizer(sf1, sf2, feat1.lock, feat2.lock)

  def anchor(w: IndexedSeq[W]):Anchoring = {
    val s1 = sf1.anchor(w)
    val s2 = sf2.anchor(w)
    val f1 = feat1.anchor(w)
    val f2 = feat1.anchor(w)
    new ProductRefinementsHandler[L, W](s1, s2) with Anchoring {
      def words = w

      def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
        val arr1 = f1.featuresForBinaryRule(begin, split, end, rule, rule1Ref(rule, ref))
        val arr2 = f2.featuresForBinaryRule(begin, split, end, rule, rule2Ref(rule, ref))
        arr1 ++ arr2.map(_ + index.rightOffset)
      }

      def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
        val arr1 = f1.featuresForUnaryRule(begin, end, rule, rule1Ref(rule, ref))
        val arr2 = f2.featuresForUnaryRule(begin, end, rule, rule2Ref(rule, ref))
        arr1 ++ arr2.map(_ + index.rightOffset)
      }

      def featuresForSpan(begin: Int, end: Int, label: Int, ref: Int) = {
        val arr1 = f1.featuresForSpan(begin, end, label, label1Ref(label, ref))
        val arr2 = f2.featuresForSpan(begin, end, label, label2Ref(label, ref))
        arr1 ++ arr2.map(_ + index.rightOffset)
      }
    }
  }
}
