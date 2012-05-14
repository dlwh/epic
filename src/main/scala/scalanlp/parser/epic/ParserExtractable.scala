package scalanlp.parser.epic

import scalala.tensor.dense.DenseVector
import scalanlp.parser.{TreeInstance, Parser}
import scalanlp.epic.{Model, ModelFactory}

/**
 *
 * @author dlwh
 */

trait ParserExtractable[L,W] {
  def extractParser(weights: DenseVector[Double]):Parser[L,W]
}

trait ParserExtractableModelFactory[L,W] extends ModelFactory[TreeInstance[L,W]] {
  type MyModel <: Model[TreeInstance[L,W]] with ParserExtractable[L,W]
}
