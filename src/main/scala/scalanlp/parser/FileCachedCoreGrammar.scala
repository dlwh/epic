package scalanlp.parser

import java.io.File

/**
 *  TODO
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

  def specialize(words: Seq[W]) = {
    cache.getOrElse(words, backoff.specialize(words))
  }
}
