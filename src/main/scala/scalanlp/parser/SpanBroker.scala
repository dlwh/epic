package scalanlp.parser

import scalanlp.io.FileIterable
import java.io.{FileInputStream, BufferedInputStream, ObjectInputStream, File}
import scalanlp.util.Index

/**
 * 
 * @author dlwh
 */
trait SpanBroker[T,W] {
  def spanForId(id: String):DerivationScorer[T, W]
}

object SpanBroker {
  def load[T, W](file: File):SpanBroker[T, W] = {
    val it = loadSpansFile[T, W](file)
    val m = Map.empty[String,DerivationScorer[T, W]] ++ it
    new SpanBroker[T, W] {
      def spanForId(id: String) = m(id)
    }
  }

  def serializeSpans[T, W](spans: Iterable[(String,DerivationScorer[T, W])], file: File) = {
    FileIterable.write(spans,file)
  }

  private def loadSpansFile[T, W](spanFile: File):Iterable[(String,DerivationScorer[T, W])] = {
    require(spanFile.exists, spanFile + " must exist!")
    new FileIterable[(String,DerivationScorer[T, W])](spanFile)
  }

  def loadSpanIndex(spanFile: File) = {
    require(spanFile.exists, spanFile + " must exist!")
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(spanFile)))
    val index = oin.readObject().asInstanceOf[Index[String]]
    oin.close()
    index
  }

  val TRAIN_SPANS_NAME = "train.spans.ser"
  val DEV_SPANS_NAME = "dev.spans.ser"
  val TEST_SPANS_NAME = "test.spans.ser"
  val XBAR_GRAMMAR_NAME = "spanindex.ser"
}