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
import breeze.linalg.Counter
import epic.trees.{UnaryRule, BinaryRule, Rule}
import epic.framework.Feature
import collection.mutable.{ArrayBuilder, ArrayBuffer}
import breeze.util.Interner

/**
 * A Featurizer turns decisions in a grammar into a set of features, possibly weighted
 *
 * This featurizes unanchored "simple annotated grammars, " which is to say not
 * lexicalized and not span.
 *
 *
 * @author dlwh
 *
 */
// TODO: unify with other kinds of featurizers somehow.
@SerialVersionUID(1)
trait Featurizer[L, W] extends Serializable {
  def featuresFor(r: Rule[L]): Array[Feature]

  def featuresFor(l: L, w: Seq[W], pos: Int): Array[Feature]

  /**should return 0.0 if we don't care about this feature. */
  def initialValueForFeature(f: Feature): Double
}

/**
 * Just returns features on the input rules
 */
class SimpleFeaturizer[L, W] extends Featurizer[L, W] {
  def featuresFor(r: Rule[L]) = Array(RuleFeature(r):Feature)
  def featuresFor(l: L, w: Seq[W], pos: Int) = Array(LexicalFeature(l, w):Feature)

  def initialValueForFeature(f: Feature) = -1.0
}

class GenFeaturizer[L, W](wGen: (Seq[W], Int)=>IndexedSeq[Feature],
                          lGen: L=>Seq[Feature] = {(x:L)=>Seq(IndicatorFeature(x))},
                          rGen: Rule[L] => Seq[Feature] = {(x: Rule[L]) => Seq(IndicatorFeature(x))}
                           ) extends Featurizer[L, W] {

  val featureInterner = Interner[Feature]

  def featuresFor(l: L, w: Seq[W], pos: Int) = {
    val result = ArrayBuilder.make[Feature]
    val wfFeats = wGen(w, pos).map(featureInterner)
    val lfFeats = lGen(l).map(featureInterner)
    result.sizeHint(wfFeats.size * lfFeats.size)
    for ( wf <- wfFeats; lf <- lfFeats) {
      result += featureInterner.intern(PairFeature(lf, wf))
    }
    result.result()
  }

  def featuresFor(r: Rule[L]) =  rGen(r).map(featureInterner).toArray

  def initialValueForFeature(f: Feature) = 0.0
}
