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
import epic.parser.{TagScorer, Lexicon}
import epic.trees.LexicalProduction

/**
 *
 * @author dlwh
 *
 */
class FeaturizedLexicon[L, L2, W](val weights: DenseVector[Double],
                                  val featureIndexer: IndexedFeaturizer[L, L2, W]) extends TagScorer[L2, W] {


  def scoreTag(l: L2, words: Seq[W], pos: Int) = {
    val w = words(pos)
    if (wordScores.contains(w, l))
      wordScores(w, l)
    else scoreUnknown(l, w)


  }

  private def scoreUnknown(label: L2, w: W): Double = {
    featureIndexer.computeWeight(featureIndexer.labelIndex(label), w, weights)
  }


  private val wordScores = Counter2[W, L2, Double]()
  // lex: recompute features
  for {
    LexicalProduction(l, word) <- featureIndexer.lexicon.knownLexicalProductions
    tagIndex <- featureIndexer.proj.labels.refinementsOf(featureIndexer.proj.labels.coarseIndex(l))
  } {
    val score = featureIndexer.computeWeight(tagIndex, word, weights)
    wordScores(word, featureIndexer.labelIndex.get(tagIndex)) = score
  }


}