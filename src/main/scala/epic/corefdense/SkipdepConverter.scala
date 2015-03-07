package epic.corefdense

import edu.berkeley.nlp.futile.fig.basic.IOUtils
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import java.io.BufferedReader
import java.io.InputStreamReader

object SkipdepConverter {

  def processLines(sentLines: Seq[String], out: PrintWriter) {
    val splitLines = sentLines.map(_.split("\\s+"))
    val words = splitLines.map(_(1))
    val labels = splitLines.map(_(7))
    // -1 so that root is -1 and everything else is 0-indexed
    val depRels = splitLines.map(_(6).toInt - 1)
    for (i <- 0 until splitLines.size) {
      val parent = depRels(i)
      if (parent >= splitLines.size) {
//        System.err.println("Parent out of range: " + i + " / " + depRels(i) + "\n" + splitLines.map(_.reduce(_ + " " + _)).reduce(_ + "\n" + _))
      } else {
        val gp = if (parent == -1) -1 else depRels(parent)
        if (gp >= splitLines.size) {
//          System.err.println("Grandparent out of range: " + parent + " / " + depRels(parent) + "\n" + splitLines.map(_.reduce(_ + " " + _)).reduce(_ + "\n" + _))
        } else {
          val gpWord = (if (gp == -1) "ROOT" else words(gp)) + "_<G>"
          val pWord = if (parent == -1) "ROOT" else words(parent)
          val label = labels(i) + "_<L>"
          out.println(label + " " + gpWord + " " + pWord + " " + words(i) + " " + label)
        }
      }
    }
  }
  
  def main(args: Array[String]) {
    require(args.size >= 1, "Need input file")
    val sentLines = new ArrayBuffer[String];
    val in = IOUtils.lineIterator(args(0))
    val out = if (args.size == 2) {
      IOUtils.openOutHard(args(1))
    } else {
      new PrintWriter(System.out)
    }
//    val out = IOUtils.openOutHard(args(1))
    var numLinesRead = 0
    while (in.hasNext()) {
      val nextLine = in.next.trim
      if (nextLine.isEmpty) {
        processLines(sentLines, out)
        sentLines.clear()
      } else {
        sentLines += nextLine
      }
      numLinesRead += 1
    }
    if (!sentLines.isEmpty) {
      processLines(sentLines, out)
    }
    out.close()
  }
}