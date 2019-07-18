package epic.lexicon

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
import math.log
import breeze.linalg._

/**
 * @param counts
 * @tparam L
 */
class SignatureTagScorer[L, String](counts: Counter2[L, String, Double], signature: String=>String) extends TagScorer[L, String] {
  def anchor(w: IndexedSeq[String]):Anchoring = new Anchoring {
    def words: IndexedSeq[String] = w
    val sigs = w.map(x => if (counts(::, x).valuesIterator.nonEmpty) x else signature(x))
    def scoreTag(pos: Int, l: L) = {
      counts(l, sigs(pos))
    }
  }
}
