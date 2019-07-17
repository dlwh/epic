package epic.preprocess

import java.io.FileInputStream

import breeze.config.CommandLineParser
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
  case class Params(splitOnNewline: Boolean = false)
  def main(_args: Array[String]):Unit = {
    val (config, args) = CommandLineParser.parseArguments(_args)
    val params = config.readIn[Params]()
    import params._

    val ins = if (args.isEmpty) IndexedSeq(System.in) else args.toStream.map(new FileInputStream(_))
    val streaming = new StreamSentenceSegmenter(MLSentenceSegmenter.bundled().get, segmentOnNewLines = params.splitOnNewline)
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