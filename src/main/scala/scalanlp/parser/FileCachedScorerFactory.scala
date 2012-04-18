package scalanlp.parser

import java.io.File

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class FileCachedScorerFactory[L, W](backoff: DerivationScorer.Factory[L,W], file: File) extends DerivationScorer.Factory[L, W] with Serializable {
  def this(grammar: Grammar[L], lexicon: Lexicon[L, W], file: File) = {
    this(DerivationScorerFactory.identity(grammar, lexicon), file)
  }

  val grammar = backoff.grammar
  val lexicon = backoff.lexicon

  private val cache = scalanlp.util.readObject[Map[Seq[W], DerivationScorer[L, W]]](file)

  def specialize(words: Seq[W]) = {
    cache.getOrElse(words, backoff.specialize(words))
  }
}
