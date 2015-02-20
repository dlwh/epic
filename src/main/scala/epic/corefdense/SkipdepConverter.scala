package epic.corefdense

import edu.berkeley.nlp.futile.fig.basic.IOUtils
import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer

object SkipdepConverter {

  def processLines(sentLines: Seq[String], out: PrintWriter) {
    val splitLines = sentLines.map(_.split("\\s+"))
    // -1 so that root is -1 and everything else is 0-indexed
    val words = splitLines.map(_(1))
    val labels = splitLines.map(_(7))
    val depRels = splitLines.map(_(6).toInt - 1)
    for (i <- 0 until splitLines.size) {
      val parent = depRels(i)
      val gp = if (parent == -1) -1 else depRels(parent)
      val gpWord = (if (gp == -1) "ROOT" else words(gp)) + "_<G>"
      val pWord = if (parent == -1) "ROOT" else words(parent)
      val label = labels(i) + "_<L>"
      out.println(label + " " + gpWord + " " + pWord + " " + words(i) + " " + label)
    }
  }
  
  def main(args: Array[String]) {
    require(args.size == 2, "Must have input and output")
    val sentLines = new ArrayBuffer[String]; 
    val in = IOUtils.lineIterator(args(0))
    val out = IOUtils.openOutHard(args(1))
    while (in.hasNext()) {
      val nextLine = in.next.trim
      if (nextLine.isEmpty) {
        processLines(sentLines, out)
        sentLines.clear()
      } else {
        sentLines += nextLine
      }
    }
    if (!sentLines.isEmpty) {
      processLines(sentLines, out)
    }
    out.close()
  }
}