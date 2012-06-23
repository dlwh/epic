package epic.parser

import java.util.concurrent.ConcurrentHashMap


/**
 * TODO: make the cache kick things out.
 * @author dlwh
 */
@SerialVersionUID(1L)
class MapCacheCoreGrammar[L, W](trueFactory: CoreGrammar[L, W]) extends CoreGrammar[L, W] with Serializable {
  private val cache = new ConcurrentHashMap[Seq[W], CoreAnchoring[L, W]]

  def grammar = trueFactory.grammar
  def lexicon = trueFactory.lexicon

  def anchor(words: Seq[W]) = {
    val r = cache.get(words)
    if (r != null) r
    else {
      val anchoring = trueFactory.anchor(words)
      cache.putIfAbsent(words, anchoring)
      anchoring
    }
  }
}
