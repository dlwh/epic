package epic.util

import breeze.util.Index
import java.io.{FileInputStream, FileOutputStream, PrintStream, File}
import breeze.linalg.DenseVector
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.io.Source

/**
 * TODO
 *
 * @author dlwh
 **/
object WeightsCache {

  def write[A](file: File, index: Index[A], weights: DenseVector[Double], threshold: Double = 1E-4) {
    require(weights.length == index.size)
    val out = new PrintStream(new GZIPOutputStream(new FileOutputStream(file), 1024))
    var i = 0
    while (i < index.size) {
      if (weights(i).abs > threshold)
        out.println(index.get(i) + "\t" + weights(i))
      i += 1
    }
    out.close()
  }

  def read[A](file: File, index: Index[A]):DenseVector[Double] = {
    val in = (for(line <- Source.fromInputStream(new GZIPInputStream(new FileInputStream(file), 1024)).getLines()) yield {
      val arr = line.split("\t")
      arr(0) -> arr(1).toDouble
    }).toMap
    DenseVector.tabulate(index.size)(i => in.getOrElse(index.get(i).toString, 0.0))
  }

}
