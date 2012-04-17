package scalanlp.parser

import java.io.File

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class FileCachedScorerFactory[L, W](val grammar: Grammar[L],
                                    val lexicon: Lexicon[L, W],
                                    file: File) extends DerivationScorer.Factory[L, W] with Serializable{
  private val cache = scalanlp.util.readObject[Map[Seq[W], DerivationScorer[L, W]]](file)

  def specialize(words: Seq[W]) = {
    cache.getOrElse(words, DerivationScorer.identity(grammar, lexicon, words))
  }
}
