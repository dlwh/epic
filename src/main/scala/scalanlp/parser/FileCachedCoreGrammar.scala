package scalanlp.parser

import java.io.File

/**
 * A CoreGrammar that relies on a file cache, which stores
 * a Map[Seq[W], CoreAnchoring] and a backoff grammar.
 * Currently, only [[scalanlp.parser.projections.ProjectTreebankToConstraints]]
 * creates these.
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class FileCachedCoreGrammar[L, W](backoff: CoreGrammar[L,W], file: File) extends CoreGrammar[L, W] with Serializable {
  def this(grammar: BaseGrammar[L], lexicon: Lexicon[L, W], file: File) = {
    this(CoreGrammar.identity(grammar, lexicon), file)
  }

  val grammar = backoff.grammar
  val lexicon = backoff.lexicon

  private val cache = scalanlp.util.readObject[Map[Seq[W], CoreAnchoring[L, W]]](file)

  def anchor(words: Seq[W]) = {
    cache.getOrElse(words, backoff.anchor(words))
  }
}
