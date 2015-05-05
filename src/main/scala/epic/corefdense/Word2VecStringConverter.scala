package epic.corefdense

import edu.berkeley.nlp.futile.fig.basic.IOUtils
import epic.dense.Word2Vec

/**
 * Applies Word2Vec.convertWord to each word of a corpus
 */
object Word2VecStringConverter {

  def main(args: Array[String]) {
    val input = args(0)
    val output = args(1)
    val lines = IOUtils.lineIterator(input)
    val out = IOUtils.openOutHard(output)
    while (lines.hasNext) {
      out.println(lines.next.split("\\s+").map(Word2Vec.convertWord(_)).foldLeft("")(_ + " " + _).trim)
    }
    out.close()
  }
}