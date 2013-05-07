package epic.parser.projections

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
import java.io.File
import epic.parser.{BaseGrammar, CoreGrammar}
import epic.parser.projections.ConstraintAnchoring.RawConstraints
import epic.lexicon.Lexicon

/**
 * A CoreGrammar that relies on a file cache, which stores
 * a Map[IndexedSeq[W], CoreAnchoring] and a backoff grammar.
 * Currently, only [[epic.parser.projections.ProjectTreebankToConstraints]]
 * creates these.
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class FileCachedCoreGrammar[L, W](backoff: CoreGrammar[L, W], file: File) extends CoreGrammar[L, W] with Serializable {
  def this(grammar: BaseGrammar[L], lexicon: Lexicon[L, W], file: File) = {
    this(CoreGrammar.identity(grammar, lexicon), file)
  }

  val grammar = backoff.grammar
  val lexicon = backoff.lexicon

  private val cache = breeze.util.readObject[Map[IndexedSeq[W], RawConstraints[L]]](file)

  def anchor(words: IndexedSeq[W]) = {
    cache.get(words).map(_.toAnchoring(grammar, lexicon, words)).getOrElse(backoff.anchor(words))
  }
}
