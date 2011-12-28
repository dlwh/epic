package scalanlp.parser

import scalanlp.io.FileIterable
import java.io.{FileInputStream, BufferedInputStream, ObjectInputStream, File}
import scalanlp.util.Index

/**
 * 
 * @author dlwh
 */

trait SpanBroker[T] {
  def spanForId(id: String):SpanScorer[T]
}

object SpanBroker {
  def load[T](file: File):SpanBroker[T] = {
    val it = loadSpansFile[T](file)
    val m = Map.empty[String,SpanScorer[T]] ++ it
    new SpanBroker[T] {
      def spanForId(id: String) = m(id)
    }
  }

  def serializeSpans[T](spans: Iterable[(String,SpanScorer[T])], file: File) = {
    FileIterable.write(spans,file)
  }

  def zeroBroker[T]:SpanBroker[T] = {
    new SpanBroker[T] {
      def spanForId(id: String) = SpanScorer.identity[T]
    }

  }

  private def loadSpans(spanDir: File) = {
    if(!spanDir.exists || !spanDir.isDirectory) sys.error(spanDir + " must exist and be a directory!")

    val trainSpans = loadSpansFile(new File(spanDir,TRAIN_SPANS_NAME))
    val devSpans = loadSpansFile(new File(spanDir,DEV_SPANS_NAME))
    val testSpans = loadSpansFile(new File(spanDir,TEST_SPANS_NAME))

    (trainSpans,devSpans,testSpans)
  }

  private def loadSpansFile[T](spanFile: File):Iterable[(String,SpanScorer[T])] = {
    require(spanFile.exists, spanFile + " must exist!")
    new FileIterable[(String,SpanScorer[T])](spanFile)
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
  val SPAN_INDEX_NAME = "spanindex.ser"
}