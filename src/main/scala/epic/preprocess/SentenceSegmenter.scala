package epic.preprocess

import epic.slab._
import epic.slab.Sentence

/**
 *
 * @author dlwh
 */
trait SentenceSegmenter extends StringAnalysisFunction[Any, Sentence] with (String => Iterable[String]) with Serializable {
  override def toString = getClass.getName



  def apply(a: String):IndexedSeq[String] = {
    val slab = Slab(a)
    apply(slab).iterator[Sentence].toIndexedSeq.map(s => slab.spanned(s._1))
  }

}
