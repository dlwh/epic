package scalanlp.parser.epic

import scalanlp.parser.Parser
import scalala.tensor.dense.DenseVector

/**
 *
 * @author dlwh
 */

trait ParserExtractable[L,W] {
  def extractParser(weights: DenseVector[Double]):Parser[L,W]
}
