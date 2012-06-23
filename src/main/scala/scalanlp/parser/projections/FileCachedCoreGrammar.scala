package scalanlp.parser.projections

import java.io.File
import scalanlp.parser.{CoreAnchoring, Lexicon, BaseGrammar, CoreGrammar}
import scalanlp.parser.projections.ConstraintAnchoring.RawConstraints

/**
 * A CoreGrammar that relies on a file cache, which stores
 * a Map[Seq[W], CoreAnchoring] and a backoff grammar.
 * Currently, only [[scalanlp.parser.projections.ProjectTreebankToConstraints]]
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

  private val cache = breeze.util.readObject[Map[Seq[W], RawConstraints]](file)

  def anchor(words: Seq[W]) = {
    cache.get(words).map(_.toAnchoring(grammar, lexicon, words)).getOrElse(backoff.anchor(words))
  }
}
