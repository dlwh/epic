package epic.lexicon

import breeze.linalg._
import breeze.util.Index
import epic.features.EnglishWordClassGenerator
import breeze.util.SerializableLogging

/**
 * A lexicon that backs off to a signature when it decides which tags to allow.
 */
@SerialVersionUID(1L)
class SignatureLexicon[L, W](val labelIndex: Index[L], allowed: Map[W, Set[Int]], signature: W => W) extends Lexicon[L, W] with Serializable with SerializableLogging {

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

object SignatureLexicon {
  def fromCounts[L, W](labelIndex: Index[L],
                       counts: Counter2[L, W, Double],
                       signatureThreshold: Double = 5,
                       sig: W=>W = EnglishWordClassGenerator) = {

    val totalCounts = sum(counts(::, *))
    val map = collection.mutable.Map[W, Set[Int]]().withDefaultValue(Set.empty)
    for( (w, total) <- totalCounts.iterator) {
      val dest = if (total >= signatureThreshold) w else sig(w)
      map(dest) ++= counts(::, w).keysIterator.map(labelIndex)
    }

    new SignatureLexicon(labelIndex, map.toMap, sig)
  }
}
