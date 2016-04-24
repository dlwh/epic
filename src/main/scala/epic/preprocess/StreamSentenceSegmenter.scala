package epic.preprocess

import java.io.{Reader, InputStream}
import breeze.util.Iterators
import java.nio.channels.Channels

/**
 * TODO
 *
 * @author dlwh
 **/
class StreamSentenceSegmenter(val baseSegmenter: SentenceSegmenter, segmentOnNewLines: Boolean = false) {

  def sentences(stream: InputStream):Iterator[String] = {
    // addendum maintains the characters that we haven't read.
    var addendum = ""
    val pieces = chunkInput(stream).flatMap { (s: String) =>
      if (segmentOnNewLines) {
        val sentences = (addendum + s).split("\n").flatMap(baseSegmenter(_)).toIndexedSeq
        if (!s.endsWith("\n")) {
          addendum = sentences.last
          sentences.dropRight(1)
        } else {
          sentences
        }
      } else {
        val sentences = baseSegmenter(addendum + s).flatMap(baseSegmenter(_)).toIndexedSeq
        addendum = sentences.last
        sentences.dropRight(1)
      }
    }

    pieces ++ Iterator(addendum).filter(_.nonEmpty)
  }

  private def chunkInput(stream: InputStream):Iterator[String] = {
    val cin = Channels.newChannel(stream)
    val reader = Channels.newReader(cin, "UTF-8")
    val buffer = new Array[Char](1024 * 1024)
    var done = false
    Iterators.fromProducer {
      if (done)  {
        None
      } else {
        val numRead = reader.read(buffer)
        if (numRead == -1) {
          done = true
          None
        } else {
          val s = new String(buffer.take(numRead))
          Some(s)
        }
      }



    }
  }

}

object StreamSentenceSegmenter {
  def main(args: Array[String]) {
    val seg = MLSentenceSegmenter.loadModel(new java.io.File("en-sent-segmenter.model.ser.gz"))
    val ss = new StreamSentenceSegmenter(seg)
    for(s <- ss.sentences(System.in)) {
      println(">>> " + s)
    }
  }
}
