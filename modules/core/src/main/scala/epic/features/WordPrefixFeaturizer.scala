package epic.features

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
import epic.framework.Feature
import breeze.linalg._
import collection.mutable.ArrayBuffer
import breeze.util.{Encoder, Index}

@SerialVersionUID(1L)
class WordPrefixFeaturizer(wordCounts: Counter[String, Double], prefixOrder: Int = 5, commonWordThreshold: Int = 100) extends WordFeaturizer[String] with Serializable {

  private val wordIndex = Index(wordCounts.keysIterator)
  private val knownWordFeatures = Encoder.fromIndex(wordIndex).tabulateArray(s => featuresFor(s).toArray)

  def anchor(w: IndexedSeq[String]): WordFeatureAnchoring[String] = new WordFeatureAnchoring[String] {
    def words: IndexedSeq[String] = w
    val indices = words.map(wordIndex)
    val myFeatures = words.indices.map(i => if (indices(i) < 0) featuresFor(words(i)).toArray else knownWordFeatures(indices(i)))
    def featuresForWord(pos: Int): Array[Feature] = {
      myFeatures(pos)
    }
  }

  def featuresFor(w: String): Array[Feature] = {
    val wc = wordCounts(w)
    if (wc > commonWordThreshold) {
      Array.empty
    } else {
      val features = new ArrayBuffer[Feature]
      val wlen = w.length
      if (wlen >= 4) {
        for(i <- 1 to ((wlen - 1) min prefixOrder)) {
          features += PrefixFeature(w.substring(0,i))
        }
      }

      features.toArray
    }
  }

  def apply(w: String) = featuresFor(w)

}


