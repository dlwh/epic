package epic.lexicon

import breeze.util.Index
import epic.util.SafeLogging

/**
 * A simple lexicon that thresholds to decide when to open up the rare word to all (open) tags
 */
@SerialVersionUID(1L)
class SignatureLexicon[L, W](val labelIndex: Index[L], allowed: Map[W, Set[Int]], signature: W => W) extends Lexicon[L, W] with Serializable with SafeLogging {

  override def morePermissive: Lexicon[L, W] = {
    new SignatureLexicon(labelIndex, Map.empty[W, Set[Int]].withDefaultValue(allTags), signature)
  }

  val allTags = allowed.values.reduce(_ ++ _)
  def anchor(w: IndexedSeq[W]):Anchoring = new Anchoring {
    def words: IndexedSeq[W] = w

    val sigs = w.map(x => allowed.get(x).orElse(allowed.get(signature(x))).getOrElse{
      logger.warn(s"unknown word signature ${signature(x)} for word $x ")
      allTags
    })

    def allowedTags(pos: Int): Set[Int] = sigs(pos)

    def length: Int = words.length
  }

}


