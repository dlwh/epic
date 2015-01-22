package epic.preprocess

import java.io.FileInputStream

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


object SegmentSentences {
  def main(args: Array[String]):Unit = {
    val in = if(args.length == 0) System.in else new FileInputStream(args(0))

    val text = scala.io.Source.fromInputStream(in).mkString

    MLSentenceSegmenter.bundled().get.apply(text).map(_.replaceAll("\n"," ")) foreach println

  }
}