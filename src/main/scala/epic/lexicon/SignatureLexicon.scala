package epic.lexicon

import breeze.linalg._
import java.io.ObjectStreamException
import breeze.util.Index
import scala.collection.immutable.BitSet
import scala.collection.mutable

/**
 * A simple lexicon that thresholds to decide when to open up the rare word to all (open) tags
 * @param wordTagCounts (tag -> word -> count)
 * @param openTagThreshold how many different word types does a tag have to be seen with to be considered open.
 * @param closedWordThreshold How many times do we have to see a word before we decide we know its tag set?
 *                            Words with counts below this will be allowed to be any open tag in addition to their
 *                            observed tag set.
 */
@SerialVersionUID(1L)
class SignatureLexicon[L, W](val labelIndex: Index[L], allowed: Map[W, Set[Int]], signature: W=>W) extends Lexicon[L, W] with Serializable {


  val allTags = allowed.values.reduce(_ ++ _)
  def anchor(w: IndexedSeq[W]):Anchoring = new Anchoring {
    def words: IndexedSeq[W] = w

    val sigs = w.map(x => allowed.get(x).orElse(allowed.get(signature(x))).getOrElse(allTags))

    def allowedTags(pos: Int): Set[Int] = sigs(pos)

    def length: Int = words.length
  }


}


