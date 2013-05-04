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
import java.util.concurrent.ConcurrentHashMap


/**
 * TODO: make the cache kick things out.
 * @author dlwh
 */
@SerialVersionUID(1L)
class MapCacheCoreGrammar[L, W](trueFactory: CoreGrammar[L, W]) extends CoreGrammar[L, W] with Serializable {
  private val cache = new ConcurrentHashMap[IndexedSeq[W], CoreAnchoring[L, W]]

  def grammar = trueFactory.grammar
  def lexicon = trueFactory.lexicon

  def anchor(words: IndexedSeq[W]) = {
    val r = cache.get(words)
    if (r != null) r
    else {
      val anchoring = trueFactory.anchor(words)
      cache.putIfAbsent(words, anchoring)
      anchoring
    }
  }
}
