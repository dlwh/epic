package epic.preprocess

import java.io.FileInputStream

import epic.slab._
import epic.slab.Sentence

object SegmentSentences {
  def main(args: Array[String]):Unit = {
    val in = if(args.length == 0) System.in else new FileInputStream(args(0))

    val text = scala.io.Source.fromInputStream(in).mkString

    MLSentenceSegmenter.bundled().get.strings(text).map(_.replaceAll("\n"," ")) foreach println

  }
}