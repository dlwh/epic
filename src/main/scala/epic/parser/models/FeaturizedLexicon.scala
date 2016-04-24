package epic.parser.models

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
import breeze.linalg._
import epic.lexicon.TagScorer

/**
 *
 * @author dlwh
 *
 */
class FeaturizedLexicon[L, L2, W](val weights: DenseVector[Double],
                                  val featureIndexer: IndexedFeaturizer[L, L2, W]) extends TagScorer[L2, W] {

  def anchor(w: IndexedSeq[W]): Anchoring = new Anchoring {
    val fi = featureIndexer.anchor(w)
    def words: IndexedSeq[W] = w

    def scoreTag(pos: Int, l: L2): Double = {
      fi.computeWeight(pos, featureIndexer.labelIndex(l), weights)

    }
  }

}