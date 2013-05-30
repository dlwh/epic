package epic.parser.features

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
import epic.trees.Rule
import epic.framework.Feature
import epic.features.IndexedWordFeaturizer

/**
 * A Featurizer turns decisions in a grammar into a set of features
 *
 * @author dlwh
 *
 */
// TODO: unify with other kinds of featurizers somehow.
@SerialVersionUID(1)
trait Featurizer[L, W] extends Serializable {
  def anchor(words: IndexedSeq[W]):Anchoring

  trait Anchoring {
    def featuresFor(begin: Int, split: Int, end: Int, r: Rule[L]): Array[Feature]
    def featuresFor(pos: Int, label: L): Array[Feature]
  }

  /**should return 0.0 if we don't care about this feature. */
  def initialValueForFeature(f: Feature): Double
}

trait SimpleFeaturizer[L, W] extends Featurizer[L, W] {
  def featuresFor(r: Rule[L]):Array[Feature]
}



class GenFeaturizer[L, W](wGen: IndexedWordFeaturizer[W],
                       lGen: L=>Seq[Feature] = {(x:L)=>Seq(IndicatorFeature(x))},
                       rGen: Rule[L] => Seq[Feature] = {(x: Rule[L]) => Seq(IndicatorFeature(x) )} ) extends SimpleFeaturizer[L, W] { outer =>

  def anchor(words: IndexedSeq[W]): Anchoring = new Anchoring {
    val lexAnch = wGen.anchor(words)
    def featuresFor(begin: Int, split: Int, end: Int, r: Rule[L]): Array[Feature] = outer.featuresFor(r)

    def featuresFor(pos: Int, label: L): Array[Feature] = {
      lexAnch.featuresForWord(pos).map(i => LexicalFeature(label, wGen.featureIndex.get(i)))
    }
  }

  def featuresFor(r: Rule[L]) =  rGen(r).toArray

  def initialValueForFeature(f: Feature) = 0.0
}
