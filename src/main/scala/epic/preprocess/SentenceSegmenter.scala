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
    val ins = if(args.length == 0) IndexedSeq(System.in) else args.toStream.map(new FileInputStream(_))
    val streaming = new StreamSentenceSegmenter(MLSentenceSegmenter.bundled().get)
    for(in <- ins) {
      try {
        for(s <- streaming.sentences(in)) {
          println(s.replaceAll("\n"," ").trim)
        }
      } finally {
        in.close()
      }
    }
  }
}